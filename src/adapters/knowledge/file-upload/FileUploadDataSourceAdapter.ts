import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { put } from "@vercel/blob";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import { DataSourceItemList } from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { FileUploadDataSourceInput } from "./types/FileUploadDataSourceInput";

export class FileUploadDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as FileUploadDataSourceInput;
    const result: DataSourceItemList = {
      dataSourceName: input.filename,
      items: [
        {
          name: input.filename,
          type: DataSourceType.FILE_UPLOAD,
        },
      ],
    };
    return result;
  }

  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    const input = data as FileUploadDataSourceInput;

    const blob = await put(input.filename, input.file, {
      access: "public",
    });
    knowledge.blobUrl = blob.url;

    const metadata = await fileLoader.loadFile(
      knowledge.id,
      input.filename,
      input.mimetype,
      input.file
    );

    return {
      indexStatus: KnowledgeIndexStatus.COMPLETED,
      metadata,
    };
  }
  retrieveKnowledgeIdFromEvent(data: any): string {
    throw new Error("Method not implemented.");
  }
  handleKnowledgeIndexedEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }

  public async pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    return {
      indexStatus: knowledge.indexStatus ?? KnowledgeIndexStatus.INITIALIZED,
    };
  }

  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    await fileLoader.deleteKnowledge(knowledgeId);
  }
}

const fileUploadDataSourceAdapter = new FileUploadDataSourceAdapter();
export default fileUploadDataSourceAdapter;
