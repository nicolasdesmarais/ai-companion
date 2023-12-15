import { DataSourceDto } from "@/src/domain/models/DataSources";

export interface ListDataSourcesResponse {
  data: DataSourceDto[];
}
export interface CreateApiDataSourceRequest {
  name: string;
  data: any;
}
