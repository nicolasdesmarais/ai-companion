import { Knowledge } from "@prisma/client";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import { DataSourceItem, DataSourceItemList } from "../types/DataSourceTypes";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { ApiDataSourceInput } from "./ApiDataSourceInput";

export class ApiDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as ApiDataSourceInput;

    const result: DataSourceItemList = {
      items: [
        {
          name: input.name,
          uniqueId: input.hash,
          originalContent: {
            contentBlobUrl: input.blobUrl,
            filename: input.name,
            mimeType: "application/json",
          },
        },
      ],
    };
    return result;
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    return true;
  }

  pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    return [];
  }
}

const apiDataSourceAdapter = new ApiDataSourceAdapter();
export default apiDataSourceAdapter;
