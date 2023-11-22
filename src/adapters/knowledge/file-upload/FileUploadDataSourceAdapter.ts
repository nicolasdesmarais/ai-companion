import { DomainEvent } from "@/src/domain/events/domain-event";
import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { put } from "@vercel/blob";
import { publishEvent } from "../../inngest/event-publisher";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import { DataSourceItemList } from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import {
  KnowledgeIndexingResult,
  KnowledgeIndexingResultStatus,
} from "../types/KnowlegeIndexingResult";
import { FileUploadDataSourceInput } from "./types/FileUploadDataSourceInput";

export class FileUploadDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as FileUploadDataSourceInput;

    const result: DataSourceItemList = {
      items: [
        {
          name: input.filename,
          type: DataSourceType.FILE_UPLOAD,
          blobUrl: input.blobUrl,
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
    const originalBlobUrl = knowledge.blobUrl;
    const response = await fetch(originalBlobUrl);
    const blob = await response.blob();

    const { docs, metadata } = await fileLoader.getLangchainDocs(
      knowledge.id,
      input.filename,
      input.mimetype,
      blob
    );

    const cloudBlob = await put(
      `${knowledge.name}.json`,
      JSON.stringify(docs),
      {
        access: "public",
      }
    );
    knowledge.blobUrl = cloudBlob.url;

    const eventIds: string[] = [];
    for (let i = 0; i < docs.length; i++) {
      const eventResult = await publishEvent(
        DomainEvent.KNOWLEDGE_CHUNK_RECEIVED,
        {
          knowledgeIndexingResult: {
            knowledgeId: knowledge.id,
            result: {
              chunkCount: docs.length,
              status: KnowledgeIndexingResultStatus.SUCCESSFUL,
            },
          },
          dataSourceType: DataSourceType.GOOGLE_DRIVE,
          index: i,
        }
      );
      eventIds.concat(eventResult.ids);
    }

    return {
      userId,
      indexStatus: KnowledgeIndexStatus.INDEXING,
      metadata: {
        mimeType: input.mimetype,
        fileName: input.filename,
        eventIds,
        originalBlobUrl,
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

  public async loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult,
    index: number
  ): Promise<IndexKnowledgeResponse> {
    if (!knowledge.blobUrl) {
      console.error("loadKnowledgeResult: blob fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const response = await fetch(knowledge.blobUrl);
    if (response.status !== 200) {
      console.error("loadKnowledgeResult: fetch fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }
    const data = await response.json();
    if (!data || !data[index]) {
      console.error("loadKnowledgeResult: data fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const docs = [data[index]];
    const metadata = await fileLoader.loadDocs(docs);

    let indexStatus;
    switch (result.status) {
      case KnowledgeIndexingResultStatus.SUCCESSFUL:
      case KnowledgeIndexingResultStatus.PARTIAL:
        indexStatus = KnowledgeIndexStatus.PARTIALLY_COMPLETED;
        break;
      case KnowledgeIndexingResultStatus.FAILED:
        indexStatus = KnowledgeIndexStatus.FAILED;
    }
    return {
      indexStatus,
      metadata: {
        ...metadata,
        completedChunks: [index],
      },
    };
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
