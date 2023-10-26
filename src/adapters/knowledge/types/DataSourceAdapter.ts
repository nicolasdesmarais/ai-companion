import { DataSourceItem } from "./DataSourceItemList";

export interface DataSourceAdapter {
  getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItem>;
}
