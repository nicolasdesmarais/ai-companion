import { DataStoreItemList } from "./DataStoreItemList";

export interface DataStoreAdapter {
  getDataStoreItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataStoreItemList>;
}
