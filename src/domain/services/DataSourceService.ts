import googleDriveDataSourceAdapter from "@/src/adapters/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import { DataSourceAdapter } from "@/src/adapters/knowledge/types/DataSourceAdapter";
import { DataSourceItemList } from "@/src/adapters/knowledge/types/DataSourceItemList";
import { IndexKnowledgeResponse } from "@/src/adapters/knowledge/types/IndexKnowledgeResponse";
import webUrlsDataSourceAdapter from "@/src/adapters/knowledge/web-urls/WebUrlsDataSourceAdapter";
import prismadb from "@/src/lib/prismadb";
import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { EntityNotFoundError } from "../errors/Errors";

export class DataSourceService {
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
      const { dataSourceId, knowledgeList } = await prismadb.$transaction(
        async (tx) => {
          return await this.createDataSourceAndKnowledgeList(
            orgId,
            ownerUserId,
            type,
            itemList
          );
        }
      );

      const knowledgeListLength = knowledgeList.length;
      for (let i = 0; i < knowledgeListLength; i++) {
        const knowledge = knowledgeList[i];
        const indexKnowledgeResponse = await dataSourceAdapter.indexKnowledge(
          orgId,
          ownerUserId,
          knowledge,
          data
        );

        await prismadb.$transaction(async (tx) => {
          this.onKnowledgeIndexed(
            dataSourceId,
            knowledge,
            indexKnowledgeResponse,
            i,
            knowledgeListLength
          );
        });
      }

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
  }

  private async createDataSourceAndKnowledgeList(
    orgId: string,
    ownerUserId: string,
    type: DataSourceType,
    itemList: DataSourceItemList
  ) {
    const dataSource = await prismadb.dataSource.create({
      data: {
        orgId,
        ownerUserId,
        name: itemList.dataSourceName,
        type,
        indexPercentage: 0,
      },
    });

    const knowledgeList = [];
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

      await prismadb.dataSourceKnowledge.create({
        data: {
          dataSourceId: dataSource.id,
          knowledgeId: knowledge.id,
        },
      });
    }

    return { dataSourceId: dataSource.id, knowledgeList };
  }

  private async onKnowledgeIndexed(
    dataSourceId: string,
    knowledge: Knowledge,
    indexKnowledgeResponse: IndexKnowledgeResponse,
    knowledgeIndex: number,
    knowledgeCount: number
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

    // Update indexPercentage only when indexKnowledgeResponse.indexStatus is COMPLETED
    if (indexKnowledgeResponse.indexStatus === KnowledgeIndexStatus.COMPLETED) {
      let indexPercentage;
      if (knowledgeCount === 0) {
        indexPercentage = 100;
      } else {
        indexPercentage = ((knowledgeIndex + 1) / knowledgeCount) * 100;
      }

      await prismadb.dataSource.update({
        where: { id: dataSourceId },
        data: { indexPercentage, lastIndexedAt: new Date() },
      });
    }
  }

  private getDataSourceAdapter(type: DataSourceType): DataSourceAdapter {
    switch (type) {
      case DataSourceType.GOOGLE_DRIVE:
        return googleDriveDataSourceAdapter;
      case DataSourceType.WEB_URL:
        return webUrlsDataSourceAdapter;
      default:
        throw new Error(`DataSourceType ${type} not supported`);
    }
  }
}

const dataSourceService = new DataSourceService();
export default dataSourceService;
