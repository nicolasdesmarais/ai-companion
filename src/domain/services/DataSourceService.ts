import googleDriveDataSourceAdapter from "@/src/adapters/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import { DataSourceAdapter } from "@/src/adapters/knowledge/types/DataSourceAdapter";
import { DataSourceItem } from "@/src/adapters/knowledge/types/DataSourceItemList";
import webUrlsDataSourceAdapter from "@/src/adapters/knowledge/web-urls/WebUrlsDataSourceAdapter";
import prismadb from "@/src/lib/prismadb";
import { DataSourceIndexStatus, DataSourceType } from "@prisma/client";

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
      const dataSource = await prismadb.$transaction(async (tx) => {
        this.createDataSourceAndKnowledges(orgId, ownerUserId, type, itemList);
      });

      return dataSource;
    } catch (err) {
      console.log(err);
      throw new Error("Failed to create data store");
    }
  }

  private async createDataSourceAndKnowledges(
    orgId: string,
    ownerUserId: string,
    type: DataSourceType,
    itemList: DataSourceItem
  ) {
    const dataSource = await prismadb.dataSource.create({
      data: {
        orgId,
        ownerUserId,
        name: itemList.dataSourceName,
        type,
        indexStatus: DataSourceIndexStatus.INDEXING,
      },
    });

    for (const item of itemList.items) {
      const createdKnowledge = await prismadb.knowledge.create({
        data: {
          name: item.name,
          type: item.type,
          metadata: item.metadata,
        },
      });

      await prismadb.dataSourceKnowledge.create({
        data: {
          dataSourceId: dataSource.id,
          knowledgeId: createdKnowledge.id,
        },
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
