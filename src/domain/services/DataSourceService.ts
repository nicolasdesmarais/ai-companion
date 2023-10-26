import googleDriveDataSourceAdapter from "@/src/adapters/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import { DataSourceAdapter } from "@/src/adapters/knowledge/types/DataSourceAdapter";
import { DataSourceItemList } from "@/src/adapters/knowledge/types/DataSourceItemList";
import webUrlsDataSourceAdapter from "@/src/adapters/knowledge/web-urls/WebUrlsDataSourceAdapter";
import prismadb from "@/src/lib/prismadb";
import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";

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
        await dataSourceAdapter.indexKnowledge(
          orgId,
          ownerUserId,
          knowledge,
          data
        );

        await prismadb.$transaction(async (tx) => {
          this.onKnowledgeIndexed(
            dataSourceId,
            knowledge,
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
          indexStatus: KnowledgeIndexStatus.INDEXING,
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
    knowledgeIndex: number,
    knowledgeCount: number
  ) {
    let indexPercentage;
    if (knowledgeCount === 0) {
      indexPercentage = 100;
    } else {
      indexPercentage = ((knowledgeIndex + 1) / knowledgeCount) * 100;
    }

    await prismadb.knowledge.update({
      where: { id: knowledge.id },
      data: {
        indexStatus: KnowledgeIndexStatus.COMPLETED,
        blobUrl: knowledge.blobUrl,
        lastIndexedAt: new Date(),
      },
    });

    await prismadb.dataSource.update({
      where: { id: dataSourceId },
      data: { indexPercentage, lastIndexedAt: new Date() },
    });
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
