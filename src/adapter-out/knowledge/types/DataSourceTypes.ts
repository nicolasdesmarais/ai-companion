export interface DataSourceItemList {
  items: DataSourceItem[];
}

export interface DataSourceItem {
  name: string;
  uniqueId?: string;
  contentBlobUrl?: string;
  metadata?: any;
}

export interface RetrieveContentResponse {
  status: RetrieveContentResponseStatus;
  contentBlobUrl?: string;
  metadata?: any;
}

export enum RetrieveContentResponseStatus {
  PENDING,
  SUCCESS,
  FAILED,
}
