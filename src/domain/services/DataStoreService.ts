import googleDriveDataStoreAdapter from "@/src/adapters/knowledge/google-drive/GoogleDriveDataStoreAdapter";
import { DataStoreAdapter } from "@/src/adapters/knowledge/types/DataStoreAdapter";
import webUrlsDataStoreAdapter from "@/src/adapters/knowledge/web-urls/WebUrlsDataStoreAdapter";
import prismadb from "@/src/lib/prismadb";
import { DataStoreIndexStatus, DataStoreType } from "@prisma/client";

export class DataStoreService {
  public async createDataStore(
    orgId: string,
    ownerUserId: string,
    type: DataStoreType,
    data: any
  ) {
    const dataStoreAdapter = this.getDataStoreAdapter(type);
    const itemList = await dataStoreAdapter.getDataStoreItemList(
      orgId,
      ownerUserId,
      data
    );

    const dataStore = await prismadb.dataStore.create({
      data: {
        orgId,
        ownerUserId,
        name: itemList.dataStoreName,
        type,
        indexStatus: DataStoreIndexStatus.INDEXING,
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

      await prismadb.dataStoreKnowledge.create({
        data: {
          dataStoreId: dataStore.id,
          knowledgeId: createdKnowledge.id,
        },
      });
    }

    return dataStore;
  }

  private getDataStoreAdapter(type: DataStoreType): DataStoreAdapter {
    switch (type) {
      case DataStoreType.GOOGLE_DRIVE:
        return googleDriveDataStoreAdapter;
      case DataStoreType.WEB_URL:
        return webUrlsDataStoreAdapter;
      default:
        throw new Error(`DataStoreType ${type} not supported`);
    }
  }
}

const dataStoreService = new DataStoreService();
export default dataStoreService;
