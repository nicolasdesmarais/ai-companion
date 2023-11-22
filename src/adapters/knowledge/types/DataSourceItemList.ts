export interface DataSourceItemList {
  items: DataSourceItem[];
}

export interface DataSourceItem {
  name: string;
  type: string;
  blobUrl?: string;
  metadata?: any;
}
