import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import {
  EntityNotFoundError,
  ForbiddenError,
} from "@/src/domain/errors/Errors";
import { DomainEvent } from "@/src/domain/events/domain-event";
import oauthTokenService from "@/src/domain/services/OAuthTokenService";
import { decryptFromBuffer } from "@/src/lib/encryptionUtils";
import prismadb from "@/src/lib/prismadb";
import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { put } from "@vercel/blob";
import axios from "axios";
import msftOAuthAdapter from "../../oauth/MsftOAuthAdapter";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import {
  DataSourceItem,
  DataSourceItemList,
} from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import {
  KnowledgeIndexingResult,
  KnowledgeIndexingResultStatus,
} from "../types/KnowlegeIndexingResult";
import { OrgAndKnowledge } from "../types/OrgAndKnowledge";

export class MsftDataSourceAdapter implements DataSourceAdapter {
  private static readonly GraphApiUrl = "https://graph.microsoft.com/v1.0";
  private static readonly ConvertibleExtensions = [
    "doc",
    "docx",
    "eml",
    "htm",
    "html",
    "msg",
    "odp",
    "ods",
    "odt",
    "pps",
    "ppsx",
    "ppt",
    "pptx",
    "rtf",
    "tif",
    "tiff",
    "xls",
    "xlsm",
    "xlsx",
  ];

  private isConvertible(filename: string): boolean {
    const ext = filename.split(".").pop();
    if (!ext) {
      return false;
    }
    return MsftDataSourceAdapter.ConvertibleExtensions.includes(ext);
  }

  private async getToken(
    userId: string,
    oauthTokenId: string
  ): Promise<string> {
    const oauthToken = await prismadb.oAuthToken.findUnique({
      where: {
        id: oauthTokenId,
      },
    });

    if (!oauthToken?.data) {
      throw new EntityNotFoundError("OAuth token not found");
    }

    if (oauthToken.userId !== userId) {
      throw new ForbiddenError("Unauthorized access to OAuth token");
    }

    const oauthTokenData = JSON.parse(decryptFromBuffer(oauthToken.data)) as {
      access_token: string;
      refresh_token: string;
    };

    const { isExistingTokenValid, refreshedToken } =
      await msftOAuthAdapter.validateToken(oauthTokenData);

    if (isExistingTokenValid) {
      return oauthTokenData.access_token;
    } else {
      await oauthTokenService.upsertToken({
        ...oauthToken,
        data: refreshedToken,
      });
      return refreshedToken.access_token;
    }
  }

  public async fetch(token: string, url: string) {
    const resp = await axios.get(MsftDataSourceAdapter.GraphApiUrl + url, {
      headers: { Authorization: `Bearer ${token}` },
    });
    return resp.data;
  }

  public async search(
    userId: string,
    oauthTokenId: string,
    searchTerm: string
  ) {
    const token = await this.getToken(userId, oauthTokenId);
    if (searchTerm === "") {
      return await this.fetch(token, "/me/drive/root/children");
    }
    return await this.fetch(token, `/me/drive/root/search(q='${searchTerm}')`);
  }

  public async children(userId: string, oauthTokenId: string, id: string) {
    const token = await this.getToken(userId, oauthTokenId);
    console.log("msft children", `/me/drive/items/${id}/children`);
    return await this.fetch(token, `/me/drive/items/${id}/children`);
  }

  /**
   * Retrieves a list of files from OneDrive
   * If the fileId is a file:
   *  - returns a list with a single item
   * If the fileId is a folder:
   *  - publishes a FOLDER_SCAN_INITIATED event to process the folder asynchronously
   *  - returns an empty list of items
   * @param orgId
   * @param userId
   * @param data
   * @returns
   */
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const token = await this.getToken(userId, data.oauthTokenId);
    const item = await this.fetch(token, `/me/drive/items/${data.fileId}`);
    if (item.file) {
      const dataSourceItem: DataSourceItem = {
        name: item.name,
        uniqueId: item.id,
        metadata: {
          fileId: item.id,
          fileName: item.name,
          mimeType: item.file.mimeType,
          modifiedTime: item.lastModifiedDateTime,
        },
      };
      return {
        items: [dataSourceItem],
      };
    }
    throw new Error("Method not implemented.");
  }

  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    if (knowledge.indexStatus === KnowledgeIndexStatus.COMPLETED) {
      return {
        indexStatus: KnowledgeIndexStatus.COMPLETED,
      };
    }
    if (!userId) {
      console.error("Missing userId");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }
    if (!data?.oauthTokenId) {
      console.error("Missing oauthTokenId");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const token = await this.getToken(userId, data.oauthTokenId);
    const item = await this.fetch(token, `/me/drive/items/${data.fileId}`);
    let response;
    if (this.isConvertible(knowledge.name)) {
      response = await fetch(
        `${MsftDataSourceAdapter.GraphApiUrl}/me/drive/items/${data.fileId}/content?format=pdf`,
        {
          headers: { Authorization: `Bearer ${token}` },
        }
      );
    } else {
      const downloadUrl = item["@microsoft.graph.downloadUrl"];
      if (!downloadUrl) {
        console.error("Missing downloadUrl");
        return {
          indexStatus: KnowledgeIndexStatus.FAILED,
        };
      }
      response = await fetch(downloadUrl);
    }
    if (!response.body || response.status !== 200) {
      console.error("msft indexKnowledge: download fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }
    const blob = await response.blob();

    const { docs, metadata } = await fileLoader.getLangchainDocs(
      knowledge.id,
      item.name,
      "",
      blob
    );

    const cloudBlob = await put(
      `${knowledge.name}.json`,
      JSON.stringify(docs),
      {
        access: "public",
      }
    );
    knowledge.blobUrl = cloudBlob.url;

    let eventIds: string[] = [];
    for (let i = 0; i < docs.length; i++) {
      const eventResult = await publishEvent(
        DomainEvent.KNOWLEDGE_CHUNK_RECEIVED,
        {
          orgId,
          knowledgeIndexingResult: {
            knowledgeId: knowledge.id,
            result: {
              chunkCount: docs.length,
              status: KnowledgeIndexingResultStatus.SUCCESSFUL,
            },
          },
          dataSourceType: DataSourceType.ONEDRIVE,
          index: i,
        }
      );
      eventIds = eventIds.concat(eventResult.ids);
    }
    return {
      userId,
      indexStatus: KnowledgeIndexStatus.INDEXING,
      documentCount: metadata.documentCount,
      tokenCount: metadata.totalTokenCount,
      metadata: {
        eventIds,
        ...metadata,
      },
    };
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    return (
      (knowledge.metadata as any)?.modifiedTime !== item.metadata.modifiedTime
    );
  }

  public retrieveOrgAndKnowledgeIdFromEvent(data: any): OrgAndKnowledge {
    throw new Error("Method not implemented.");
  }

  getKnowledgeResultFromEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<KnowledgeIndexingResult> {
    throw new Error("Method not supported.");
  }

  public async loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult,
    index: number
  ): Promise<IndexKnowledgeResponse> {
    if (!knowledge.blobUrl) {
      console.error("msft loadKnowledgeResult: blob fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const response = await fetch(knowledge.blobUrl);
    if (response.status !== 200) {
      console.error("msft loadKnowledgeResult: fetch fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }
    const data = await response.json();
    if (!data || !data[index]) {
      console.error("msft loadKnowledgeResult: data fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const docs = [data[index]];
    const metadata = await fileLoader.loadDocs(docs);

    console.log("msft chunk loading", knowledge.id, index);

    let indexStatus;
    switch (result.status) {
      case KnowledgeIndexingResultStatus.SUCCESSFUL:
      case KnowledgeIndexingResultStatus.PARTIAL:
        indexStatus = KnowledgeIndexStatus.PARTIALLY_COMPLETED;
        break;
      case KnowledgeIndexingResultStatus.FAILED:
        indexStatus = KnowledgeIndexStatus.FAILED;
    }
    return {
      indexStatus,
      metadata: {
        ...metadata,
        completedChunks: [index],
      },
    };
  }

  public async pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    return {
      indexStatus: knowledge.indexStatus ?? KnowledgeIndexStatus.INITIALIZED,
    };
  }

  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    await fileLoader.deleteKnowledge(knowledgeId);
  }

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    //TODO: Implement logic to identify removed knowledge
    return [];
  }
}

const msftDataSourceAdapter = new MsftDataSourceAdapter();
export default msftDataSourceAdapter;
