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

  public async search(
    orgId: string,
    userId: string,
    oauthTokenId: string,
    searchTerms: string[]
  ) {
    const token = await this.getToken(userId, oauthTokenId);
    const resp = await axios.get(
      `${MsftDataSourceAdapter.GraphApiUrl}/me/drive/root/children`,
      {
        headers: { Authorization: `Bearer ${token}` },
      }
    );
    return resp.data;
  }

  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList> {
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
