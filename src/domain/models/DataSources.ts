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
  type: string;
  uniqueId: string | null;
  indexStatus: KnowledgeIndexStatus | null;
  documentCount: number | null;
  tokenCount: number | null;
  originalContent: KnowledgeOriginalContent | null;
}
