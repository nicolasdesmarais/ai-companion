import { DataSourceType, Knowledge } from "@prisma/client";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import { DataSourceItemList } from "../types/DataSourceItemList";
import { WebUrlDataSourceInput } from "./types/WebUrlDataSourceInput";

export class WebUrlsDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as WebUrlDataSourceInput;
    const result: DataSourceItemList = {
      dataSourceName: input.url,
      items: [
        {
          name: input.url,
          type: DataSourceType.WEB_URL,
        },
      ],
    };
    return result;
  }

  indexKnowledge(
    orgId: string,
    useId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<void> {
    throw new Error("Method not implemented.");
  }
}

const webUrlsDataSourceAdapter = new WebUrlsDataSourceAdapter();
export default webUrlsDataSourceAdapter;
