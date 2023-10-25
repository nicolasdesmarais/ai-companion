import googleDriveDataStoreAdapter from "@/src/adapters/knowledge/google-drive/GoogleDriveDataStoreAdapter";
import { DataStoreAdapter } from "@/src/adapters/knowledge/types/DataStoreAdapter";
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
    dataStoreAdapter?.getDataStoreKnowledgeList(orgId, ownerUserId, data);

    return dataStore;
  }

  private getDataStoreAdapter(
    type: DataStoreType
  ): DataStoreAdapter | undefined {
    switch (type) {
      case DataStoreType.GOOGLE_DRIVE:
        return googleDriveDataStoreAdapter;
    }
  }
}

const dataStoreService = new DataStoreService();
export default dataStoreService;
