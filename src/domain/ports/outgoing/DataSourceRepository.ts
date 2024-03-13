import {
  DataSourceRefreshPeriod,
  DataSourceType,
  Prisma,
} from "@prisma/client";
import { DataSourceDto, DataSourceFilter } from "../../models/DataSources";
import { AIDataUsage } from "../../models/OrgUsage";

export interface DataSourceRepository {
  findById(id: string): Promise<DataSourceDto | null>;
  getById(id: string): Promise<DataSourceDto>;
  findAll(filter?: DataSourceFilter): Promise<DataSourceDto[]>;
  findByOrgId(
    orgId: string,
    filter?: DataSourceFilter
  ): Promise<DataSourceDto[]>;
  findByOrgIdAndUserId(
    orgId: string,
    userId: string,
    filter?: DataSourceFilter
  ): Promise<DataSourceDto[]>;
  findByAiId(aiId: string): Promise<DataSourceDto[]>;
  findDataSourceIdsToRefresh(now: Date): Promise<string[]>;

  initializeDataSource(
    orgId: string,
    ownerUserId: string,
    name: string,
    type: DataSourceType,
    refreshPeriod: DataSourceRefreshPeriod,
    data: any
  ): Promise<DataSourceDto>;

  updateDataSource(
    id: string,
    input: Prisma.DataSourceUpdateInput
  ): Promise<DataSourceDto>;

  getNumberOfTokensStoredForOrg(orgId: string): Promise<number>;

  getNumberOfTokensStoredForOrgPerAi(orgId: string): Promise<AIDataUsage[]>;

  updateDataSourceAis(dataSourceId: string, aiIds: string[]): Promise<void>;

  deleteUnusedKnowledges(): Promise<string[]>;

  getOriginalDataSourceIdForKnowledge(
    knowledgeId: string
  ): Promise<string | null>;

  updateDataSourceData(
    datasourceId: string,
    metadata: any
  ): Promise<DataSourceDto>;
}
