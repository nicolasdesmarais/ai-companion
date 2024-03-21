import { Knowledge, KnowledgeIndexStatus } from "@prisma/client";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import { DataSourceItem, DataSourceItemList } from "../types/DataSourceTypes";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { FileUploadDataSourceInput } from "./types/FileUploadDataSourceInput";

export class FileUploadDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as FileUploadDataSourceInput;

    let originalContent;
    if (input.blobUrl) {
      originalContent = {
        contentBlobUrl: input.blobUrl,
        filename: input.filename,
        mimeType: input.mimetype,
      };
    }

    const result: DataSourceItemList = {
      items: [
        {
          name: input.filename,
          uniqueId: input.blobUrl,
          originalContent,
        },
      ],
    };
    return result;
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    return (
      knowledge.uniqueId !== item.uniqueId ||
      knowledge.indexStatus !== KnowledgeIndexStatus.COMPLETED
    );
  }

  public async pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    return {
      indexStatus: knowledge.indexStatus ?? KnowledgeIndexStatus.INITIALIZED,
    };
  }

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    return [];
  }
}

const fileUploadDataSourceAdapter = new FileUploadDataSourceAdapter();
export default fileUploadDataSourceAdapter;
