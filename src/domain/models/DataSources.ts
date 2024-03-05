import { KnowledgeOriginalContent } from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import {
  DataSourceIndexStatus,
  DataSourceRefreshPeriod,
  DataSourceType,
  KnowledgeIndexStatus,
} from "@prisma/client";

export interface DataSourceDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  lastIndexedAt: Date | null;
  orgId: string;
  ownerUserId: string;
  name: string;
  type: DataSourceType;
  refreshPeriod: DataSourceRefreshPeriod | null;
  indexStatus: DataSourceIndexStatus | null;
  indexPercentage: string;
  data: any;
}

export interface DataSourceFilter {
  search?: string;
  type?: DataSourceType;
  orderBy?: DataSourceOrderBy;
}

export interface DataSourceOrderBy {
  field: DataSourceOrderByField;
  direction: DataSourceOrderByDirection;
}

export enum DataSourceOrderByField {
  CREATED_AT = "createdAt",
  LAST_INDEXED_AT = "lastIndexedAt",
  NAME = "name",
  TYPE = "type",
  PROGRESS = "indexPercentage",
}

export enum DataSourceOrderByDirection {
  ASC = "asc",
  DESC = "desc",
}

export interface KnowledgeDto {
  id: string;
  name: string;
  type: DataSourceType;
  uniqueId: string | null;
  indexStatus: KnowledgeIndexStatus;
  documentCount: number | null;
  tokenCount: number | null;
  originalContent: KnowledgeOriginalContent | null;
  documentsBlobUrl: string | null;
  indexPercentage: string;
  metadata: any;
}

export interface KnowledgeChunkDto {
  chunkNumber: number;
  startIndex: number | null;
  endIndex: number | null;
}

export interface KnowledgeChunkCounts {
  totalCount: number;
  completedCount: number;
  failedCount: number;
}

export interface KnowledgeCounts {
  totalCount: number;
  indexingCount: number;
  completedCount: number;
  partiallyCompletedCount: number;
  failedCount: number;
  totalDocumentCount: number;
  totalTokenCount: number;
  indexPercentage: number;
}

export const knowldedgeEndStatuses: KnowledgeIndexStatus[] = [
  KnowledgeIndexStatus.COMPLETED,
  KnowledgeIndexStatus.PARTIALLY_COMPLETED,
  KnowledgeIndexStatus.FAILED,
];

export interface KnowledgeSummary {
  documentCount: number;
  tokenCount: number;
  knowledgeIds: string[];
}
