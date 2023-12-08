export interface DataSourceItemList {
  type: string;
  items: DataSourceItem[];
}

export interface DataSourceItem {
  name: string;
  uniqueId?: string;
  blobUrl?: string;
  metadata?: any;
}
