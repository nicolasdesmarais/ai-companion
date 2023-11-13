import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { put } from "@vercel/blob";
import { readFile } from "fs/promises";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import { DataSourceItemList } from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { KnowledgeIndexingResult } from "../types/KnowlegeIndexingResult";
import { FileUploadDataSourceInput } from "./types/FileUploadDataSourceInput";
import { Readable } from "stream";
const { finished } = require("stream/promises");
import fs from "fs";

export class FileUploadDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as FileUploadDataSourceInput;
    const file = await readFile(input.filepath);
    const blob = await put(input.filename, file, {
      access: "public",
    });
    const result: DataSourceItemList = {
      dataSourceName: input.filename,
      items: [
        {
          name: input.filename,
          type: DataSourceType.FILE_UPLOAD,
          blobUrl: blob.url,
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

    if (!knowledge.blobUrl) {
      throw new Error("Missing blobUrl in knowledge");
    }
    const response = await fetch(knowledge.blobUrl);
    if (!response.body) {
      throw new Error("Failed to fetch blob");
    }
    const fileStream = fs.createWriteStream(`/tmp/${input.filename}`, {
      flags: "r+",
    });
    await finished(Readable.fromWeb(response.body as any).pipe(fileStream));

    const metadata = await fileLoader.loadFile(
      knowledge.id,
      input.filename,
      input.mimetype,
      input.filepath
    );

    return {
      indexStatus: KnowledgeIndexStatus.COMPLETED,
      metadata: {
        mimeType: input.mimetype,
        fileName: input.filename,
        ...metadata,
      },
    };
  }
  retrieveKnowledgeIdFromEvent(data: any): string {
    throw new Error("Method not implemented.");
  }
  getKnowledgeResultFromEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<KnowledgeIndexingResult> {
    throw new Error("Method not supported.");
  }

  loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult,
    chunkCount: number
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not supported.");
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
