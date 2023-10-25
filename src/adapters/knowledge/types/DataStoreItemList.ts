export interface DataStoreItemList {
  dataStoreName: string;
  items: DataStoreItem[];
}

export interface DataStoreItem {
  name: string;
  type: string;
  metadata?: any;
}
