import { DataStoreKnowledgeList } from "./DataStoreKnowledgeList";

export interface DataStoreAdapter {
  getDataStoreKnowledgeList(
    orgId: string,
    userId: string,
    data: any
  ): DataStoreKnowledgeList;
}
