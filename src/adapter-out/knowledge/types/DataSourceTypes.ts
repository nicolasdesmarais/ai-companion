import { KnowledgeIndexStatus } from "@prisma/client";

export interface DataSourceItemList {
  items: DataSourceItem[];
}

export interface DataSourceItem {
  name: string;
  uniqueId?: string;
  contentBlobUrl?: string;
  metadata?: any;
}

export interface RetrieveContentAdapterResponse {
  status: RetrieveContentResponseStatus;
  contentBlobUrl?: string;
  metadata?: any;
}

export enum RetrieveContentResponseStatus {
  PENDING,
  SUCCESS,
  FAILED,
}

export interface RetrieveContentResponse {
  indexStatus: KnowledgeIndexStatus;
  contentBlobUrl: string | null;
  metadata?: any;
}
