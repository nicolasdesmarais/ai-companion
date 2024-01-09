import { DataSourceType } from "@prisma/client";
import { DataSourceDto } from "../../models/DataSources";

export interface DataSourceRepository {
  findById(id: string): Promise<DataSourceDto | null>;
  findAll(): Promise<DataSourceDto[]>;
  findByOrgId(orgId: string): Promise<DataSourceDto[]>;
  findByOrgIdAndUserId(orgId: string, userId: string): Promise<DataSourceDto[]>;
  findByAiId(aiId: string): Promise<DataSourceDto[]>;

  initializeDataSource(
    orgId: string,
    ownerUserId: string,
    name: string,
    type: DataSourceType,
    data: any
  ): Promise<DataSourceDto>;

  updateDataSource(dataSourceDto: DataSourceDto): Promise<DataSourceDto>;
}
