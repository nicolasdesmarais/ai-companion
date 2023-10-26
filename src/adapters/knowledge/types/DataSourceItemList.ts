export interface DataSourceItem {
  dataSourceName: string;
  items: DataSourceItem[];
}

export interface DataSourceItem {
  name: string;
  type: string;
  metadata?: any;
}
