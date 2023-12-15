import { DataSourceDto } from "@/src/domain/models/DataSources";
import { DataSourceRepository } from "@/src/domain/ports/outgoing/DataSourceRepository";
import prismadb from "@/src/lib/prismadb";
import { DataSource, Prisma } from "@prisma/client";

const dataSourceSummarySelect: Prisma.DataSourceSelect = {
  id: true,
  createdAt: true,
  updatedAt: true,
  lastIndexedAt: true,
  name: true,
  type: true,
  orgId: true,
  ownerUserId: true,
  indexStatus: true,
  indexPercentage: true,
};

export class DataSourceRepositoryImpl implements DataSourceRepository {
  public async findAll(): Promise<DataSourceDto[]> {
    const dataSources = await prismadb.dataSource.findMany({
      select: dataSourceSummarySelect,
    });
    return this.mapDataSourcesToDto(dataSources);
  }
  public async findByOrgId(orgId: string): Promise<DataSourceDto[]> {
    const dataSources = await prismadb.dataSource.findMany({
      select: dataSourceSummarySelect,
      where: {
        orgId,
      },
    });
    return this.mapDataSourcesToDto(dataSources);
  }
  public async findByOrgIdAndUserId(
    orgId: string,
    userId: string
  ): Promise<DataSourceDto[]> {
    const dataSources = await prismadb.dataSource.findMany({
      select: dataSourceSummarySelect,
      where: {
        orgId,
        ownerUserId: userId,
      },
    });
    return this.mapDataSourcesToDto(dataSources);
  }

  public async findByAiId(aiId: string): Promise<DataSourceDto[]> {
    const dataSources = await prismadb.dataSource.findMany({
      select: dataSourceSummarySelect,
      where: {
        ais: {
          some: {
            aiId,
          },
        },
      },
    });
    return this.mapDataSourcesToDto(dataSources);
  }

  private mapDataSourcesToDto(dataSources: DataSource[]): DataSourceDto[] {
    return dataSources.map((dataSource) => this.mapDataSourceToDto(dataSource));
  }

  private mapDataSourceToDto(dataSource: DataSource): DataSourceDto {
    return {
      ...dataSource,
      indexPercentage: dataSource.indexPercentage.toString(),
    };
  }
}
