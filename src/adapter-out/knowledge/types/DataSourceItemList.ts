export interface DataSourceItemList {
  items: DataSourceItem[];
}

export interface DataSourceItem {
  name: string;
  uniqueId?: string;
  contentBlobUrl?: string;
  metadata?: any;
}
