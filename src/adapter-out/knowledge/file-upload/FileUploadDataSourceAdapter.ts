import { DomainEvent } from "@/src/domain/events/domain-event";
import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { put } from "@vercel/blob";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import {
  DataSourceItem,
  DataSourceItemList,
} from "../types/DataSourceItemList";
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
      type: DataSourceType.FILE_UPLOAD,
      items: [
        {
          name: input.filename,
          blobUrl: input.blobUrl,
          uniqueId: input.fileHash,
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

    const events = [];
    for (let i = 0; i < docs.length; i++) {
      const event = {
        id: `kn-chunk-${knowledge.id}-${i}`,
        name: DomainEvent.KNOWLEDGE_CHUNK_RECEIVED,
        data: {
          knowledgeIndexingResult: {
            knowledgeId: knowledge.id,
            result: {
              chunkCount: docs.length,
              status: KnowledgeIndexingResultStatus.SUCCESSFUL,
            },
          },
          dataSourceType: DataSourceType.FILE_UPLOAD,
          index: i,
        },
      };
      events.push(event);
    }

    return {
      userId,
      indexStatus: KnowledgeIndexStatus.INDEXING,
      events,
      metadata: {
        mimeType: input.mimetype,
        fileName: input.filename,
        originalBlobUrl,
        ...metadata,
      },
    };
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    return knowledge.uniqueId !== item.uniqueId;
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
