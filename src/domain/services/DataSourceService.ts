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
    type: DataSourceType,
    data: any
  ) {
    const dataSourceAdapter = this.getDataSourceAdapter(type);
    const itemList = await dataSourceAdapter.getDataSourceItemList(
      orgId,
      ownerUserId,
      data
    );

    const { dataSourceId, knowledgeList } =
      await this.createDataSourceAndKnowledgeList(
        orgId,
        ownerUserId,
        type,
        itemList
      );

    const processKnowledgeList = async () => {
      const knowledgeListLength = knowledgeList.length;
      for (let i = 0; i < knowledgeListLength; i++) {
        const knowledge = knowledgeList[i];

        let indexKnowledgeResponse;
        try {
          indexKnowledgeResponse = await dataSourceAdapter.indexKnowledge(
            orgId,
            ownerUserId,
            knowledge,
            data
          );
        } catch (error) {
          indexKnowledgeResponse = {
            indexStatus: KnowledgeIndexStatus.FAILED,
          };
        }

        await this.onKnowledgeIndexed(knowledge, indexKnowledgeResponse);
      }

      await this.updateDataSourceStatus(dataSourceId);
    };

    await processKnowledgeList().catch((error) => {
      console.log("Error in background task:", error);
    });

    return dataSourceId;
  }

  public async handleKnowledgeIndexedEvent(type: DataSourceType, data: any) {
    const dataSourceAdapter = this.getDataSourceAdapter(type);
    const knowledgeId = dataSourceAdapter.retrieveKnowledgeIdFromEvent(data);
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }

    const indexKnowledgeResponse =
      await dataSourceAdapter.handleKnowledgeIndexedEvent(knowledge, data);
    await prismadb.knowledge.update({
      where: { id: knowledge.id },
      data: {
        indexStatus: indexKnowledgeResponse.indexStatus,
      },
    });

    await this.updateCompletedKnowledgeDataSources(knowledge.id);
  }

  private async createDataSourceAndKnowledgeList(
    orgId: string,
    ownerUserId: string,
    type: DataSourceType,
    itemList: DataSourceItemList
  ) {
    console.log(
      `Creating data source and knowledge list for ${itemList.dataSourceName}, with ${itemList.items.length} items`
    );

    return await prismadb.$transaction(async (tx) => {
      try {
        console.log("Creating data source");
        const dataSource = await tx.dataSource.create({
          data: {
            orgId,
            ownerUserId,
            name: itemList.dataSourceName,
            type,
            indexStatus: DataSourceIndexStatus.INDEXING,
            indexPercentage: 0,
          },
        });

        console.log(`Data source created with id=${dataSource.id}`);

        const knowledgeList = [];
        const dataSourceKnowledgeRelations = [];

        let i = 1;
        for (const item of itemList.items) {
          console.log(
            `Creating knowledge for item ${item.name}, ${i++} of ${
              itemList.items.length
            }`
          );
          const knowledge = await tx.knowledge.create({
            data: {
              name: item.name,
              type: item.type,
              indexStatus: KnowledgeIndexStatus.INITIALIZED,
              metadata: item.metadata,
            },
          });
          console.log(`Knowledge created with id=${knowledge.id}`);

          knowledgeList.push(knowledge);

          dataSourceKnowledgeRelations.push({
            dataSourceId: dataSource.id,
            knowledgeId: knowledge.id,
          });
        }

        console.log("Creating data source knowledge relations");
        await tx.dataSourceKnowledge.createMany({
          data: dataSourceKnowledgeRelations,
        });
        console.log("Data source knowledge relations created");

        return { dataSourceId: dataSource.id, knowledgeList };
      } catch (e) {
        console.log(e);
        throw e;
      }
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
