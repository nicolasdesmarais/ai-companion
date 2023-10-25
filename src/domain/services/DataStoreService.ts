import googleDriveDataStoreAdapter from "@/src/adapters/knowledge/google-drive/GoogleDriveDataStoreAdapter";
import { DataStoreAdapter } from "@/src/adapters/knowledge/types/DataStoreAdapter";
import webUrlsDataStoreAdapter from "@/src/adapters/knowledge/web-urls/WebUrlsDataStoreAdapter";
import prismadb from "@/src/lib/prismadb";
import { DataStoreIndexStatus, DataStoreType } from "@prisma/client";

export class DataStoreService {
  public async createDataStore(
    orgId: string,
    ownerUserId: string,
    name: string,
    type: DataStoreType,
    data: any
  ) {
    const dataStore = await prismadb.dataStore.create({
      data: {
        orgId,
        ownerUserId,
        name,
        type,
        indexStatus: DataStoreIndexStatus.INITIALIZED,
      },
    });

    const dataStoreAdapter = this.getDataStoreAdapter(type);
    const knowledgeList = await dataStoreAdapter.getDataStoreKnowledgeList(
      orgId,
      ownerUserId,
      data
    );

    for (const knowledge of knowledgeList.knowledges) {
      const createdKnowledge = await prismadb.knowledge.create({
        data: {
          userId: ownerUserId,
          name: knowledge.name,
          type: knowledge.type,
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
