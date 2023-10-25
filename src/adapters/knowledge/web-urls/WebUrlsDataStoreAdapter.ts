import { DataStoreAdapter } from "../types/DataStoreAdapter";
import { DataStoreKnowledgeList } from "../types/DataStoreKnowledgeList";

export class WebUrlsDataStoreAdapter implements DataStoreAdapter {
  getDataStoreKnowledgeList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataStoreKnowledgeList> {
    throw new Error("Method not implemented.");
  }
}

const webUrlsDataStoreAdapter = new WebUrlsDataStoreAdapter();
export default webUrlsDataStoreAdapter;
