import {
  DataSourceDto,
  DataSourceFilter,
} from "@/src/domain/models/DataSources";
import { AIDataUsage } from "@/src/domain/models/OrgUsage";
import { DataSourceRepository } from "@/src/domain/ports/outgoing/DataSourceRepository";
import prismadb from "@/src/lib/prismadb";
import {
  DataSource,
  DataSourceIndexStatus,
  DataSourceRefreshPeriod,
  DataSourceType,
  Prisma,
} from "@prisma/client";

const dataSourceSummarySelect: Prisma.DataSourceSelect = {
  id: true,
  createdAt: true,
  updatedAt: true,
  lastIndexedAt: true,
  name: true,
  type: true,
  orgId: true,
  ownerUserId: true,
  refreshPeriod: true,
  indexStatus: true,
  indexPercentage: true,
  data: true,
  knowledges: {
    select: {
      knowledge: true,
    },
  },
  ais: {
    select: {
      ai: true,
    },
  },
};

const dataSourceFilterWhereClause = (
  filter?: DataSourceFilter
): Prisma.DataSourceWhereInput => {
  let whereClause: Prisma.DataSourceWhereInput = {};
  whereClause.indexStatus = {
    not: DataSourceIndexStatus.DELETED,
  };

  if (filter) {
    if (filter.search) {
      whereClause.name = {
        search: filter.search,
      };
    }

    if (filter.type) {
      whereClause.type = filter.type;
    }
  }

  return whereClause;
};

const dataSourceFilterOrderBy = (
  filter?: DataSourceFilter
): Prisma.DataSourceOrderByWithRelationAndSearchRelevanceInput | undefined => {
  return filter?.orderBy
    ? {
        [filter.orderBy.field]: filter.orderBy.direction,
      }
    : { name: "asc" };
};

interface TokenCountResult {
  total_token_count: number;
}

interface AIDataUsageResult {
  ai_id: string;
  total_token_count: number;
}

export class DataSourceRepositoryImpl implements DataSourceRepository {
  public async findById(id: string): Promise<DataSourceDto | null> {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id },
    });

    if (!dataSource) {
      return null;
    }

    return this.mapDataSourceToDto(dataSource);
  }

  public async findAll(filter?: DataSourceFilter): Promise<DataSourceDto[]> {
    const dataSources = await prismadb.dataSource.findMany({
      select: dataSourceSummarySelect,
      where: dataSourceFilterWhereClause(filter),
      orderBy: dataSourceFilterOrderBy(filter),
    });
    return this.mapDataSourcesToDto(dataSources);
  }
  public async findByOrgId(
    orgId: string,
    filter?: DataSourceFilter
  ): Promise<DataSourceDto[]> {
    const dataSources = await prismadb.dataSource.findMany({
      select: dataSourceSummarySelect,
      where: {
        orgId,
        AND: dataSourceFilterWhereClause(filter),
      },
      orderBy: dataSourceFilterOrderBy(filter),
    });
    return this.mapDataSourcesToDto(dataSources);
  }
  public async findByOrgIdAndUserId(
    orgId: string,
    userId: string,
    filter?: DataSourceFilter
  ): Promise<DataSourceDto[]> {
    const dataSources = await prismadb.dataSource.findMany({
      select: dataSourceSummarySelect,
      where: {
        orgId,
        ownerUserId: userId,
        AND: dataSourceFilterWhereClause(filter),
      },
      orderBy: dataSourceFilterOrderBy(filter),
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
        indexStatus: {
          not: DataSourceIndexStatus.DELETED,
        },
      },
    });
    return this.mapDataSourcesToDto(dataSources);
  }

  public async findDataSourceIdsToRefresh(now: Date): Promise<string[]> {
    const oneDayAgo = new Date(now);
    oneDayAgo.setDate(now.getDate() - 1);
    const oneWeekAgo = new Date(now);
    oneWeekAgo.setDate(now.getDate() - 7);
    const oneMonthAgo = new Date(now);
    oneMonthAgo.setMonth(now.getMonth() - 1);

    const dataSources = await prismadb.dataSource.findMany({
      select: {
        id: true,
      },
      where: {
        indexStatus: {
          not: DataSourceIndexStatus.DELETED,
        },
        OR: [
          {
            AND: [
              { refreshPeriod: DataSourceRefreshPeriod.DAILY },
              { lastIndexedAt: { lt: oneDayAgo } },
            ],
          },
          {
            AND: [
              { refreshPeriod: DataSourceRefreshPeriod.WEEKLY },
              { lastIndexedAt: { lt: oneWeekAgo } },
            ],
          },
          {
            AND: [
              { refreshPeriod: DataSourceRefreshPeriod.MONTHLY },
              { lastIndexedAt: { lt: oneMonthAgo } },
            ],
          },
        ],
        NOT: { refreshPeriod: DataSourceRefreshPeriod.NEVER },
      },
    });

    return dataSources.map((dataSource) => dataSource.id);
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

  public async initializeDataSource(
    orgId: string,
    ownerUserId: string,
    name: string,
    type: DataSourceType,
    refreshPeriod: DataSourceRefreshPeriod,
    data: any
  ): Promise<DataSourceDto> {
    const dataSource = await prismadb.dataSource.create({
      data: {
        orgId,
        ownerUserId,
        name,
        type,
        refreshPeriod,
        indexStatus: DataSourceIndexStatus.INITIALIZED,
        indexPercentage: 0,
        data,
      },
    });
    return this.mapDataSourceToDto(dataSource);
  }

  public async updateDataSource(
    dataSourceDto: DataSourceDto
  ): Promise<DataSourceDto> {
    const dataSource = await prismadb.dataSource.update({
      where: {
        id: dataSourceDto.id,
      },
      data: {
        ...dataSourceDto,
      },
    });
    return this.mapDataSourceToDto(dataSource);
  }

  public async getNumberOfTokensStoredForOrg(orgId: string): Promise<number> {
    const result = await prismadb.$queryRaw<TokenCountResult[]>`
    SELECT SUM(dk.token_count) as total_token_count
    FROM (
        SELECT DISTINCT k.id, k.token_count
        FROM data_sources d
        INNER JOIN data_source_knowledges dsk ON dsk.data_source_id = d.id
        INNER JOIN knowledge k ON k.id = dsk.knowledge_id
        WHERE d.org_id = ${orgId}
    ) as dk;
`;

    if (result.length > 0 && result[0].total_token_count) {
      return Number(result[0].total_token_count);
    }

    return 0;
  }

  public async getNumberOfTokensStoredForOrgPerAi(
    orgId: string
  ): Promise<AIDataUsage[]> {
    const result = await prismadb.$queryRaw<AIDataUsageResult[]>`
    SELECT ai_id, IFNULL(SUM(dk.token_count), 0) AS total_token_count
    FROM (
    SELECT DISTINCT ai.id as ai_id,
                    k.id as k_id,
                    k.token_count
    FROM ais ai
    LEFT OUTER JOIN ai_data_sources ads ON ads.ai_id = ai.id
    LEFT OUTER JOIN data_source_knowledges dsk ON dsk.data_source_id = ads.data_source_id
    LEFT OUTER JOIN knowledge k ON k.id = dsk.knowledge_id
    WHERE ai.org_id = ${orgId} ) AS dk
    GROUP BY ai_id
    ORDER BY total_token_count desc;
`;

    if (result.length === 0) {
      return [];
    }

    const aiUsages: AIDataUsage[] = result.map((row) => ({
      aiId: row.ai_id,
      aiDataTokensUsed: row.total_token_count,
    }));

    return aiUsages;
  }

  public async updateDataSourceAis(dataSourceId: string, aiIds: string[]) {
    await prismadb.$transaction(async (tx) => {
      await tx.aIDataSource.deleteMany({
        where: { dataSourceId },
      });
      await tx.aIDataSource.createMany({
        data: aiIds.map((aiId) => ({
          aiId,
          dataSourceId,
        })),
        skipDuplicates: true,
      });
    });
  }

  public async deleteUnusedKnowledges(): Promise<string[]> {
    const knowledgesWithoutDataSource = await prismadb.knowledge.findMany({
      where: {
        indexStatus: {
          not: "DELETED",
        },
        dataSources: {
          none: {},
        },
      },
      select: {
        id: true,
      },
    });

    const knowledgeIds = knowledgesWithoutDataSource.map(
      (knowledge) => knowledge.id
    );

    await prismadb.knowledge.updateMany({
      where: {
        id: {
          in: knowledgeIds,
        },
      },
      data: {
        indexStatus: "DELETED",
      },
    });

    return knowledgeIds;
  }
}
