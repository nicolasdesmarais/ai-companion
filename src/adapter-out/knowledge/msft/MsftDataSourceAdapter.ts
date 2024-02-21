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
  RetrieveContentAdapterResponse,
  RetrieveContentResponseStatus,
} from "../types/DataSourceTypes";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import {
  KnowledgeIndexingResult,
  KnowledgeIndexingResultStatus,
} from "../types/KnowlegeIndexingResult";

export enum MsftEvent {
  ONEDRIVE_FOLDER_SCAN_INITIATED = "onedrive.folder.scan.initiated",
}
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
    data: any,
    forRefresh: boolean,
    forceRefresh: boolean
  ): Promise<DataSourceItemList> {
    const token = await this.getToken(userId, data.oauthTokenId);
    const item = await this.fetch(token, `/me/drive/items/${data.fileId}`);
    const items = await this.extractDataSourceItemFromFile(
      item,
      userId,
      dataSourceId,
      data.oauthTokenId,
      forRefresh,
      forceRefresh
    );
    return {
      items,
    };
  }

  public async retrieveContent(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<RetrieveContentAdapterResponse> {
    throw new Error("Method not implemented.");
  }

  public async retrieveKnowledgeContent(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<RetrieveContentAdapterResponse> {
    if (!userId) {
      console.error("Missing userId");
      return {
        status: RetrieveContentResponseStatus.FAILED,
        metadata: {
          errors: {
            knowledge: "Missing userId",
          },
        },
      };
    }
    if (!data?.oauthTokenId) {
      console.error("Missing oauthTokenId");
      return {
        status: RetrieveContentResponseStatus.FAILED,
        metadata: {
          errors: {
            knowledge: "Missing oauthTokenId",
          },
        },
      };
    }

    const { fileId } = knowledge.metadata as any;

    if (!fileId) {
      console.error("Missing fileId");
      return {
        status: RetrieveContentResponseStatus.FAILED,
        metadata: {
          errors: {
            knowledge: "Missing fileId",
          },
        },
      };
    }

    const token = await this.getToken(userId, data.oauthTokenId);
    const item = await this.fetch(token, `/me/drive/items/${fileId}`);
    let response;
    if (this.isConvertible(knowledge.name)) {
      response = await fetch(
        `${MsftDataSourceAdapter.GraphApiUrl}/me/drive/items/${fileId}/content?format=pdf`,
        {
          headers: { Authorization: `Bearer ${token}` },
        }
      );
    } else {
      const downloadUrl = item["@microsoft.graph.downloadUrl"];
      if (!downloadUrl) {
        console.error("Missing downloadUrl");
        return {
          status: RetrieveContentResponseStatus.FAILED,
          metadata: {
            errors: {
              knowledge: "Missing downloadUrl",
            },
          },
        };
      }
      response = await fetch(downloadUrl);
    }
    if (!response.body || response.status !== 200) {
      console.error("msft indexKnowledge: download fail");
      return {
        status: RetrieveContentResponseStatus.FAILED,
        metadata: {
          errors: {
            knowledge: "msft download fail",
          },
        },
      };
    }

    const msftResponseBlob = await response.blob();
    const contentBlob = await put(fileId, msftResponseBlob, {
      access: "public",
    });

    return {
      status: RetrieveContentResponseStatus.SUCCESS,
      contentBlobUrl: contentBlob.url,
    };
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
        metadata: {
          errors: {
            knowledge: "Missing userId",
          },
        },
      };
    }
    if (!data?.oauthTokenId) {
      console.error("Missing oauthTokenId");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
        metadata: {
          errors: {
            knowledge: "Missing oauthTokenId",
          },
        },
      };
    }
    const { fileId } = knowledge.metadata as any;

    if (!fileId) {
      console.error("Missing fileId");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
        metadata: {
          errors: {
            knowledge: "Missing fileId",
          },
        },
      };
    }

    const token = await this.getToken(userId, data.oauthTokenId);
    const item = await this.fetch(token, `/me/drive/items/${fileId}`);
    let response;
    if (this.isConvertible(knowledge.name)) {
      response = await fetch(
        `${MsftDataSourceAdapter.GraphApiUrl}/me/drive/items/${fileId}/content?format=pdf`,
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
          metadata: {
            errors: {
              knowledge: "Missing downloadUrl",
            },
          },
        };
      }
      response = await fetch(downloadUrl);
    }
    if (!response.body || response.status !== 200) {
      console.error("msft indexKnowledge: download fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
        metadata: {
          errors: {
            knowledge: "msft download fail",
          },
        },
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
    const metadata = await fileLoader.loadDocs(docs, index);

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
    const parentFolderIds = new Set(
      dataSourceItemList.items
        .filter((item) => item.metadata?.parentFolderId)
        .map((item) => item.metadata.parentFolderId)
    );

    const uniqueIds = dataSourceItemList.items
      .map((item) => item.uniqueId)
      .filter((uniqueId) => uniqueId !== undefined) as string[];

    const removedKnowledgeIds: string[] = [];
    for (const folderId of Array.from(parentFolderIds)) {
      const folderRemovedKnowledgeIds = await prismadb.knowledge.findMany({
        select: {
          id: true,
        },
        where: {
          uniqueId: {
            not: {
              in: uniqueIds,
            },
          },
          metadata: {
            path: "$.parentFolderId",
            equals: folderId,
          },
        },
      });
      folderRemovedKnowledgeIds.forEach((knowledge) =>
        removedKnowledgeIds.push(knowledge.id)
      );
    }
    return removedKnowledgeIds;
  }

  /**
   * Retrieves a list of files from a folder.
   * Each file in the folder is returned as a DataSourceItem, with all relevant metadata.
   * For each child folder in the folder, a FOLDER_SCAN_INITIATED event is published
   * to process the folder asynchronously.
   * @param orgId
   * @param userId
   * @param oauthTokenId
   * @param dataSourceId
   * @param folderId
   * @returns
   */
  public async getDataSourceItemListFromFolder(
    userId: string,
    oauthTokenId: string,
    dataSourceId: string,
    folderId: string,
    forRefresh: boolean,
    forceRefresh: boolean
  ): Promise<DataSourceItemList> {
    const token = await this.getToken(userId, oauthTokenId);
    const children = await this.fetch(
      token,
      `/me/drive/items/${folderId}/children`
    );

    if (!children) {
      return {
        items: [],
      };
    }

    const files: DataSourceItem[] = [];
    for (const file of children.value) {
      const items = await this.extractDataSourceItemFromFile(
        file,
        userId,
        dataSourceId,
        oauthTokenId,
        forRefresh,
        forceRefresh,
        folderId
      );
      files.push(...items);
    }

    return {
      items: files,
    };
  }

  private async extractDataSourceItemFromFile(
    item: any,
    userId: string,
    dataSourceId: string,
    oauthTokenId: string,
    forRefresh: boolean,
    forceRefresh: boolean,
    parentFolderId?: string
  ): Promise<DataSourceItem[]> {
    if (item.file) {
      const dataSourceItem: DataSourceItem = {
        name: item.name,
        uniqueId: item.id,
        metadata: {
          fileId: item.id,
          fileName: item.name,
          mimeType: item.file.mimeType,
          modifiedTime: item.lastModifiedDateTime,
          parentFolderId,
        },
      };
      return [dataSourceItem];
    }
    if (item.folder) {
      await publishEvent(MsftEvent.ONEDRIVE_FOLDER_SCAN_INITIATED, {
        userId,
        oauthTokenId: oauthTokenId,
        dataSourceId,
        folderId: item.id,
        forRefresh,
        forceRefresh,
      });
      return [];
    }
    throw new Error("Unknown MSFT item type.");
  }
}

const msftDataSourceAdapter = new MsftDataSourceAdapter();
export default msftDataSourceAdapter;
