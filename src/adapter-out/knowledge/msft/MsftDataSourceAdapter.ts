import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import {
  DataSourceItem,
  DataSourceItemList,
} from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { KnowledgeIndexingResult } from "../types/KnowlegeIndexingResult";
import axios from "axios";
import prismadb from "@/src/lib/prismadb";
import {
  EntityNotFoundError,
  ForbiddenError,
} from "@/src/domain/errors/Errors";
import { decryptFromBuffer } from "@/src/lib/encryptionUtils";

export class MsftDataSourceAdapter implements DataSourceAdapter {
  private static readonly GraphApiUrl = "https://graph.microsoft.com/v1.0";

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

    return oauthTokenData.access_token;
  }

  private async fetch(token: string, url: string) {
    const resp = await axios.get(MsftDataSourceAdapter.GraphApiUrl + url, {
      headers: { Authorization: `Bearer ${token}` },
    });
    return resp.data;
  }

  public async search(
    orgId: string,
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
      console.log(item, dataSourceItem);
    }
    throw new Error("Method not implemented.");
    return {
      type: DataSourceType.ONEDRIVE,
      items: [],
    };
  }

  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    throw new Error("Method not implemented.");
  }

  public retrieveKnowledgeIdFromEvent(data: any): string {
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
    throw new Error("Method not implemented.");
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
    throw new Error("Method not implemented.");
  }
}

const msftDataSourceAdapter = new MsftDataSourceAdapter();
export default msftDataSourceAdapter;
