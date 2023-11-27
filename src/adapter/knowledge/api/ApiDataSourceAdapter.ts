import { CreateApiDataSourceRequest } from "@/src/domain/ports/api/DataSourcesApi";
import {
  $Enums,
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
  Prisma,
} from "@prisma/client";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import { DataSourceItemList } from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { KnowledgeIndexingResult } from "../types/KnowlegeIndexingResult";

export class ApiDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as CreateApiDataSourceRequest;

    const result: DataSourceItemList = {
      items: [
        {
          name: data.name,
          type: DataSourceType.API,
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
      metadata: {
        documentCount,
        totalTokenCount,
      },
    };
  }
  retrieveKnowledgeIdFromEvent(data: any): string {
    throw new Error("Method not implemented.");
  }
  getKnowledgeResultFromEvent(
    knowledge: {
      id: string;
      createdAt: Date;
      updatedAt: Date;
      lastIndexedAt: Date | null;
      userId: string | null;
      name: string;
      type: string;
      indexStatus: $Enums.KnowledgeIndexStatus | null;
      blobUrl: string | null;
      metadata: Prisma.JsonValue;
    },
    data: any
  ): Promise<KnowledgeIndexingResult> {
    throw new Error("Method not implemented.");
  }
  loadKnowledgeResult(
    knowledge: {
      id: string;
      createdAt: Date;
      updatedAt: Date;
      lastIndexedAt: Date | null;
      userId: string | null;
      name: string;
      type: string;
      indexStatus: $Enums.KnowledgeIndexStatus | null;
      blobUrl: string | null;
      metadata: Prisma.JsonValue;
    },
    result: KnowledgeIndexingResult,
    chunkCount: number
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }
  pollKnowledgeIndexingStatus(knowledge: {
    id: string;
    createdAt: Date;
    updatedAt: Date;
    lastIndexedAt: Date | null;
    userId: string | null;
    name: string;
    type: string;
    indexStatus: $Enums.KnowledgeIndexStatus | null;
    blobUrl: string | null;
    metadata: Prisma.JsonValue;
  }): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }
  deleteKnowledge(knowledgeId: string): Promise<void> {
    throw new Error("Method not implemented.");
  }
}

const apiDataSourceAdapter = new ApiDataSourceAdapter();
export default apiDataSourceAdapter;