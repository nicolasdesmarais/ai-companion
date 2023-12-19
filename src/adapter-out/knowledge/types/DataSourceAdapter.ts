import { Knowledge } from "@prisma/client";
import { DataSourceItem, DataSourceItemList } from "./DataSourceItemList";
import { IndexKnowledgeResponse } from "./IndexKnowledgeResponse";
import { KnowledgeIndexingResult } from "./KnowlegeIndexingResult";

export interface DataSourceAdapter {
  getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList>;

  indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse>;

  shouldReindexKnowledge(knowledge: Knowledge, item: DataSourceItem): boolean;

  retrieveKnowledgeIdFromEvent(data: any): string;

  getKnowledgeResultFromEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<KnowledgeIndexingResult>;

  loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult,
    chunkCount: number
  ): Promise<IndexKnowledgeResponse>;

  pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse>;

  deleteKnowledge(knowledgeId: string): Promise<void>;
}
