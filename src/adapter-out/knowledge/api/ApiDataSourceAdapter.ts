import { CreateApiDataSourceRequest } from "@/src/adapter-in/api/DataSourcesApi";
import { Knowledge, KnowledgeIndexStatus } from "@prisma/client";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import {
  DataSourceItem,
  DataSourceItemList,
} from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { KnowledgeIndexingResult } from "../types/KnowlegeIndexingResult";

export class ApiDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as CreateApiDataSourceRequest;

    const result: DataSourceItemList = {
      items: [
        {
          name: data.name,
        },
      ],
    };
    return result;
  }
  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    const input = data as CreateApiDataSourceRequest;

    const { documentCount, totalTokenCount } = await fileLoader.loadJsonArray(
      [input.data],
      knowledge.id
    );

    return {
      indexStatus: KnowledgeIndexStatus.COMPLETED,
      documentCount: documentCount,
      tokenCount: totalTokenCount,
      metadata: {
        documentCount,
        totalTokenCount,
      },
    };
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    return true;
  }

  public retrieveKnowledgeIdFromEvent(data: any): string {
    throw new Error("Method not implemented.");
  }
  public async getKnowledgeResultFromEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<KnowledgeIndexingResult> {
    throw new Error("Method not implemented.");
  }
  loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult,
    chunkCount: number
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }
  pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }
  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    await fileLoader.deleteKnowledge(knowledgeId);
  }

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    return [];
  }
}

const apiDataSourceAdapter = new ApiDataSourceAdapter();
export default apiDataSourceAdapter;
