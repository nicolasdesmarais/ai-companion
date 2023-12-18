import { DataSourceDto } from "../../models/DataSources";

export interface DataSourceRepository {
  findAll(): Promise<DataSourceDto[]>;
  findByOrgId(orgId: string): Promise<DataSourceDto[]>;
  findByOrgIdAndUserId(orgId: string, userId: string): Promise<DataSourceDto[]>;
  findByAiId(aiId: string): Promise<DataSourceDto[]>;
}
