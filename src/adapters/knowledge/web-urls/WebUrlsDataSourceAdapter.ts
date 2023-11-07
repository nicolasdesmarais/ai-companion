import { BadRequestError } from "@/src/domain/errors/Errors";
import {
  ApifySupportedEvents,
  ApifyWebhookEvent,
} from "@/src/domain/types/ApifyWebhookEvent";
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
import apifyAdapter from "./ApifyAdapter";
import { WebUrlDataSourceInput } from "./types/WebUrlDataSourceInput";
import { WebUrlMetadata } from "./types/WebUrlMetadata";

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

  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    const input = data as WebUrlDataSourceInput;
    console.log(`Indexing web url ${input.url}`);
    const actorRunId = await apifyAdapter.startUrlIndexing(
      knowledge.id,
      input.url
    );

    if (!actorRunId) {
      throw new Error("Failed to start web indexing run");
    }

    console.log(`Started web indexing run ${actorRunId}`);

    const metadata: WebUrlMetadata = {
      indexingRunId: actorRunId,
    };

    return {
      indexStatus: KnowledgeIndexStatus.INDEXING,
      metadata,
    };
  }

  public retrieveKnowledgeIdFromEvent(data: any): string {
    const event = data as ApifyWebhookEvent;
    return event.knowledgeId;
  }

  public async handleKnowledgeIndexedEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    const event = data as ApifyWebhookEvent;
    const metadata = knowledge.metadata as unknown as WebUrlMetadata;
    const actorRunId = event.eventData.actorRunId;
    if (actorRunId !== metadata.indexingRunId) {
      throw new BadRequestError("Event actorRunId does not match metadata");
    }

    if (event.eventType !== ApifySupportedEvents.ACTOR_RUN_SUCCEEDED) {
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    return await this.getActorRunResult(knowledge, metadata);
  }

  public async pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    const metadata = knowledge.metadata as unknown as WebUrlMetadata;
    if (!metadata?.indexingRunId) {
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    return this.getActorRunResult(knowledge, metadata);
  }

  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    fileLoader.deleteKnowledge(knowledgeId);
  }

  private async getActorRunResult(
    knowledge: Knowledge,
    metadata: WebUrlMetadata
  ): Promise<IndexKnowledgeResponse> {
    console.log(
      `Retrieving actor run result for indexingRunId=${metadata.indexingRunId}`
    );
    const result = await apifyAdapter.getActorRunResult(metadata.indexingRunId);
    if (!result.isSuccessful || !result.items) {
      console.log(
        `Actor run for indexingRunId=${metadata.indexingRunId} was unsuccessful`
      );
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const { documentCount, totalTokenCount } = await fileLoader.loadFile(
      knowledge.id,
      knowledge.name,
      "text/csv",
      this.bufferToBlob(result.items)
    );

    const cloudBlob = await put(
      `${knowledge.name}.json`,
      JSON.stringify(result),
      {
        access: "public",
      }
    );
    knowledge.blobUrl = cloudBlob.url;
    return {
      indexStatus: KnowledgeIndexStatus.COMPLETED,
      blobUrl: cloudBlob.url,
      metadata: {
        ...metadata,
        documentCount,
        totalTokenCount,
      },
    };
  }

  private bufferToBlob(buffer: Buffer): Blob {
    const arrayBuffer = new ArrayBuffer(buffer.length);
    const view = new Uint8Array(arrayBuffer);
    for (let i = 0; i < buffer.length; ++i) {
      view[i] = buffer[i];
    }

    const blob = new Blob([arrayBuffer]);

    return blob;
  }
}

const webUrlsDataSourceAdapter = new WebUrlsDataSourceAdapter();
export default webUrlsDataSourceAdapter;
