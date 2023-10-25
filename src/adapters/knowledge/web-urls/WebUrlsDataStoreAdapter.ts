import { DataStoreType } from "@prisma/client";
import { DataStoreAdapter } from "../types/DataStoreAdapter";
import { DataStoreItemList } from "../types/DataStoreItemList";
import { WebUrlDataStoreInput } from "./types/WebUrlDataStoreInput";

export class WebUrlsDataStoreAdapter implements DataStoreAdapter {
  public async getDataStoreItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataStoreItemList> {
    const input = data as WebUrlDataStoreInput;
    const result: DataStoreItemList = {
      dataStoreName: input.url,
      items: [
        {
          name: input.url,
          type: DataStoreType.WEB_URL,
        },
      ],
    };
    return result;
  }
}

const webUrlsDataStoreAdapter = new WebUrlsDataStoreAdapter();
export default webUrlsDataStoreAdapter;
