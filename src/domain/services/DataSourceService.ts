import { inngest } from "@/src/adapters/inngest/client";
import fileUploadDataSourceAdapter from "@/src/adapters/knowledge/file-upload/FileUploadDataSourceAdapter";
import googleDriveDataSourceAdapter from "@/src/adapters/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import { DataSourceAdapter } from "@/src/adapters/knowledge/types/DataSourceAdapter";
import { DataSourceItemList } from "@/src/adapters/knowledge/types/DataSourceItemList";
import { IndexKnowledgeResponse } from "@/src/adapters/knowledge/types/IndexKnowledgeResponse";
import webUrlsDataSourceAdapter from "@/src/adapters/knowledge/web-urls/WebUrlsDataSourceAdapter";
import prismadb from "@/src/lib/prismadb";
import {
  DataSourceIndexStatus,
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { EntityNotFoundError } from "../errors/Errors";

export class DataSourceService {
  private getDataSourceAdapter(type: DataSourceType): DataSourceAdapter {
    switch (type) {
      case DataSourceType.GOOGLE_DRIVE:
        return googleDriveDataSourceAdapter;
      case DataSourceType.WEB_URL:
        return webUrlsDataSourceAdapter;
      case DataSourceType.FILE_UPLOAD:
        return fileUploadDataSourceAdapter;
      default:
        throw new Error(`DataSourceType ${type} not supported`);
    }
  }

  public async getDataSources(orgId: string, userId: string, aiId: string) {
    return await prismadb.dataSource.findMany({
      where: {
        orgId,
        ownerUserId: userId,
        ais: {
          some: {
            aiId,
          },
        },
      },
    });
  }

  public async createDataSource(
    orgId: string,
    ownerUserId: string,
    name: string,
    type: DataSourceType,
    data: any
  ) {
    const dataSourceId = await this.persistDataSource(
      orgId,
      ownerUserId,
      name,
      type,
      data
    );

    await inngest.send({
      name: "datasources/datasource.persisted",
      data: {
        dataSourceId,
        dataSourceType: type,
      },
    });

    return dataSourceId;
  }

  public async getDataSourceKnowledgeList(dataSourceId: string) {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
    });

    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    const dataSourceAdapter = this.getDataSourceAdapter(dataSource.type);
    const itemList = await dataSourceAdapter.getDataSourceItemList(
      dataSource.orgId,
      dataSource.ownerUserId,
      dataSource.data
    );

    await this.persistKnowledge(dataSourceId, itemList);
  }

  public async indexDataSourceKnowlege(dataSourceId: string) {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
    });

    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }
    const dataSourceAdapter = this.getDataSourceAdapter(dataSource.type);

    const knowledgeList = await prismadb.knowledge.findMany({
      where: {
        dataSources: {
          some: {
            dataSourceId: dataSource.id,
          },
        },
      },
    });

    for (const knowledge of knowledgeList) {
      let indexKnowledgeResponse;
      try {
        indexKnowledgeResponse = await dataSourceAdapter.indexKnowledge(
          dataSource.orgId,
          dataSource.ownerUserId,
          knowledge,
          dataSource.data
        );
      } catch (error) {
        console.log(error);
        indexKnowledgeResponse = {
          indexStatus: KnowledgeIndexStatus.FAILED,
        };
      }

      await this.onKnowledgeIndexed(knowledge, indexKnowledgeResponse);
    }

    await this.updateDataSourceStatus(dataSourceId);

    return dataSourceId;
  }

  public async handleKnowledgeIndexedEvent(type: DataSourceType, data: any) {
    console.log("Received knowledge indexed event");
    const dataSourceAdapter = this.getDataSourceAdapter(type);
    const knowledgeId = dataSourceAdapter.retrieveKnowledgeIdFromEvent(data);
    console.log(`Updating knowledge ${knowledgeId}`);
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }

    console.log(`Found knowledge ${knowledgeId}`);

    const indexKnowledgeResponse =
      await dataSourceAdapter.handleKnowledgeIndexedEvent(knowledge, data);

    await this.onKnowledgeIndexed(knowledge, indexKnowledgeResponse);
    await this.updateCompletedKnowledgeDataSources(knowledge.id);
  }

  public async pollDataSourceStatus() {
    console.log("Polling data source status");
    const currentDate = new Date();
    const oneHourAgo = new Date(currentDate.getTime() - 60 * 60 * 1000);

    const dataSources = await prismadb.dataSource.findMany({
      take: 1,
      where: {
        indexStatus: DataSourceIndexStatus.INDEXING,
        updatedAt: {
          lt: oneHourAgo,
        },
        type: DataSourceType.WEB_URL, // limit to WEB_URLs for now
      },
    });

    if (dataSources.length === 0) {
      return;
    }

    const dataSource = dataSources[0];
    const knowledges = await prismadb.knowledge.findMany({
      where: {
        indexStatus: {
          in: [KnowledgeIndexStatus.INITIALIZED, KnowledgeIndexStatus.INDEXING],
        },
        dataSources: {
          some: {
            dataSourceId: dataSource.id,
          },
        },
      },
    });

    console.log(`Polling data source ${dataSource.id}`);
    const dataSourceAdapter = this.getDataSourceAdapter(dataSource.type);
    for (const knowledge of knowledges) {
      const indexKnowledgeResponse =
        await dataSourceAdapter.pollKnowledgeIndexingStatus(knowledge);

      await this.onKnowledgeIndexed(knowledge, indexKnowledgeResponse);
    }

    await this.updateDataSourceStatus(dataSource.id);
  }

  private async persistDataSource(
    orgId: string,
    ownerUserId: string,
    name: string,
    type: DataSourceType,
    data: any
  ) {
    const dataSource = await prismadb.dataSource.create({
      data: {
        orgId,
        ownerUserId,
        name,
        type,
        indexStatus: DataSourceIndexStatus.INDEXING,
        indexPercentage: 0,
        data,
      },
    });
    return dataSource.id;
  }

  private async persistKnowledge(
    dataSourceId: string,
    itemList: DataSourceItemList
  ) {
    const knowledgeList = [];
    const dataSourceKnowledgeRelations = [];

    for (const item of itemList.items) {
      const knowledge = await prismadb.knowledge.create({
        data: {
          name: item.name,
          type: item.type,
          indexStatus: KnowledgeIndexStatus.INITIALIZED,
          metadata: item.metadata,
        },
      });
      knowledgeList.push(knowledge);

      dataSourceKnowledgeRelations.push({
        dataSourceId,
        knowledgeId: knowledge.id,
      });
    }

    await prismadb.dataSourceKnowledge.createMany({
      data: dataSourceKnowledgeRelations,
    });
  }

  private async onKnowledgeIndexed(
    knowledge: Knowledge,
    indexKnowledgeResponse: IndexKnowledgeResponse
  ) {
    let updateDataForKnowledge: {
      indexStatus: KnowledgeIndexStatus;
      blobUrl: string | null;
      lastIndexedAt: Date;
      metadata?: any;
    } = {
      indexStatus: indexKnowledgeResponse.indexStatus,
      blobUrl: knowledge.blobUrl,
      lastIndexedAt: new Date(),
    };

    if (indexKnowledgeResponse.metadata) {
      const currentKnowledge = await prismadb.knowledge.findUnique({
        where: { id: knowledge.id },
        select: { metadata: true },
      });

      if (!currentKnowledge) {
        return;
      }

      const currentMetadata =
        currentKnowledge.metadata &&
        typeof currentKnowledge.metadata === "object"
          ? currentKnowledge.metadata
          : {};

      // Merge existing metadata with the new metadata
      updateDataForKnowledge.metadata = {
        ...currentMetadata,
        ...indexKnowledgeResponse.metadata,
      };
    }

    await prismadb.knowledge.update({
      where: { id: knowledge.id },
      data: updateDataForKnowledge,
    });
  }

  private async updateCompletedKnowledgeDataSources(knowledgeId: string) {
    const dataSourceIds = await prismadb.dataSource.findMany({
      select: { id: true },
      where: {
        knowledges: { some: { knowledgeId } },
      },
    });

    for (const dataSource of dataSourceIds) {
      await this.updateDataSourceStatus(dataSource.id);
    }
  }

  private async updateDataSourceStatus(dataSourceId: string) {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
      include: {
        knowledges: {
          include: {
            knowledge: true,
          },
        },
      },
    });

    if (!dataSource) {
      return;
    }

    const knowledgeCount = dataSource.knowledges.length;
    let completedKnowledges = 0;
    let failedKnowledges = 0;
    for (const knowledge of dataSource.knowledges) {
      if (knowledge.knowledge.indexStatus === KnowledgeIndexStatus.COMPLETED) {
        completedKnowledges++;
      } else if (
        knowledge.knowledge.indexStatus === KnowledgeIndexStatus.FAILED
      ) {
        failedKnowledges++;
      }
    }

    let indexPercentage;
    if (knowledgeCount === 0) {
      indexPercentage = 100;
    } else {
      indexPercentage = (completedKnowledges / knowledgeCount) * 100;
    }

    let indexingStatus;
    if (failedKnowledges > 0) {
      indexingStatus = DataSourceIndexStatus.FAILED;
    } else if (completedKnowledges === knowledgeCount) {
      indexingStatus = DataSourceIndexStatus.COMPLETED;
    } else {
      indexingStatus = DataSourceIndexStatus.INDEXING;
    }

    await prismadb.dataSource.update({
      where: { id: dataSource.id },
      data: {
        indexStatus: indexingStatus,
        indexPercentage,
        lastIndexedAt: new Date(),
      },
    });
  }

  public async deleteDataSource(aiId: string, dataSourceId: string) {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
      include: {
        knowledges: true,
      },
    });
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    const dataSourceAdapter = this.getDataSourceAdapter(dataSource.type);
    const knowledgeIds: string[] = [];
    for (const knowledge of dataSource.knowledges) {
      await dataSourceAdapter.deleteKnowledge(knowledge.knowledgeId);
      knowledgeIds.push(knowledge.knowledgeId);
    }

    await prismadb.$transaction(async (tx) => {
      await prismadb.aIDataSource.deleteMany({
        where: { dataSourceId },
      });

      await prismadb.dataSourceKnowledge.deleteMany({
        where: { dataSourceId },
      });

      await prismadb.knowledge.deleteMany({
        where: { id: { in: knowledgeIds } },
      });

      await prismadb.dataSource.delete({ where: { id: dataSourceId } });
    });
  }
}

const dataSourceService = new DataSourceService();
export default dataSourceService;
