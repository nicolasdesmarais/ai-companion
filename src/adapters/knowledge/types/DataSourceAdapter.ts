import { Knowledge } from "@prisma/client";
import { DataSourceItemList } from "./DataSourceItemList";
import { IndexKnowledgeResponse } from "./IndexKnowledgeResponse";

export interface DataSourceAdapter {
  getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItemList>;

  indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse>;

  retrieveKnowledgeIdFromEvent(data: any): string;

  handleKnowledgeIndexedEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse>;

  pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse>;

  deleteKnowledge(knowledgeId: string): Promise<void>;
}
