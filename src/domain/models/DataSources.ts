import {
  DataSourceIndexStatus,
  DataSourceRefreshPeriod,
  DataSourceType,
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
  USAGE_COUNT = "usageCount",
}

export enum DataSourceOrderByDirection {
  ASC = "asc",
  DESC = "desc",
}
