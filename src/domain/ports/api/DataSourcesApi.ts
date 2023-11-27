import { DataSourceIndexStatus, DataSourceType } from "@prisma/client";

export interface GetDataSourcesResponse {
  data: DataSourceDto[];
}

export interface DataSourceDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  lastIndexedAt: Date | null;
  name: string;
  type: DataSourceType;
  indexStatus: DataSourceIndexStatus | null;
  indexPercentage: string;
}

export interface CreateApiDataSourceRequest {
  name: string;
  data: any;
}
