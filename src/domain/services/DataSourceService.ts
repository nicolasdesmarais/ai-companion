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

    try {
      const { dataSourceId, knowledgeList } =
        await this.createDataSourceAndKnowledgeList(
          orgId,
          ownerUserId,
          type,
          itemList
        );

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

        await this.onKnowledgeIndexed(
          dataSourceId,
          knowledge,
          indexKnowledgeResponse
        );
      }

      await this.updateDataSourceStatus(dataSourceId);

      return dataSourceId;
    } catch (err) {
      console.log(err);
      throw new Error("Failed to create data store");
    }
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

    if (indexKnowledgeResponse.indexStatus === KnowledgeIndexStatus.COMPLETED) {
      this.updateCompletedKnowledgeDataSources(knowledge.id);
    }
  }

  private async createDataSourceAndKnowledgeList(
    orgId: string,
    ownerUserId: string,
    type: DataSourceType,
    itemList: DataSourceItemList
  ) {
    return await prismadb.$transaction(async (tx) => {
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

      const knowledgeList = [];
      for (const item of itemList.items) {
        const knowledge = await tx.knowledge.create({
          data: {
            name: item.name,
            type: item.type,
            indexStatus: KnowledgeIndexStatus.INITIALIZED,
            metadata: item.metadata,
          },
        });
        knowledgeList.push(knowledge);

        await tx.dataSourceKnowledge.create({
          data: {
            dataSourceId: dataSource.id,
            knowledgeId: knowledge.id,
          },
        });
      }

      return { dataSourceId: dataSource.id, knowledgeList };
    });
  }

  private async onKnowledgeIndexed(
    dataSourceId: string,
    knowledge: Knowledge,
    indexKnowledgeResponse: IndexKnowledgeResponse
  ) {
    let updateDataForKnowledge = {
      indexStatus: indexKnowledgeResponse.indexStatus,
      blobUrl: knowledge.blobUrl,
      lastIndexedAt: new Date(),
      metadata: typeof indexKnowledgeResponse.metadata,
    };

    // Update the metadata field only if it's present in indexKnowledgeResponse
    if (indexKnowledgeResponse.metadata) {
      updateDataForKnowledge.metadata = indexKnowledgeResponse.metadata;
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
      this.updateDataSourceStatus(dataSource.id);
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
