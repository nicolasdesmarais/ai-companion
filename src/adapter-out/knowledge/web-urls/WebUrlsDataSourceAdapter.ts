import { BadRequestError } from "@/src/domain/errors/Errors";
import { ApifyWebhookEvent } from "@/src/domain/models/ApifyWebhookEvent";
import { logWithTimestamp } from "@/src/lib/logging";
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
import apifyAdapter from "./ApifyAdapter";
import { WebUrlDataSourceInput } from "./types/WebUrlDataSourceInput";
import { WebUrlMetadata } from "./types/WebUrlMetadata";

export class WebUrlsDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as WebUrlDataSourceInput;
    const result: DataSourceItemList = {
      type: DataSourceType.WEB_URL,
      items: [
        {
          name: input.url,
          uniqueId: input.url,
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
    const actorRunId = await apifyAdapter.startUrlIndexing(
      knowledge.id,
      input.url
    );

    if (!actorRunId) {
      throw new Error("Failed to start web indexing run");
    }

    const metadata: WebUrlMetadata = {
      indexingRunId: actorRunId,
    };

    return {
      indexStatus: KnowledgeIndexStatus.INDEXING,
      metadata,
    };
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    if (knowledge.uniqueId !== item.uniqueId) {
      return true;
    }

    const oneWeekAgo = new Date();
    oneWeekAgo.setDate(oneWeekAgo.getDate() - 7);
    return !knowledge.lastIndexedAt || knowledge.lastIndexedAt < oneWeekAgo;
  }

  public retrieveKnowledgeIdFromEvent(data: any): string {
    const event = data as ApifyWebhookEvent;
    return event.knowledgeId;
  }

  public async getKnowledgeResultFromEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<KnowledgeIndexingResult> {
    const event = data as ApifyWebhookEvent;
    const metadata = knowledge.metadata as unknown as WebUrlMetadata;
    const actorRunId = event.eventData.actorRunId;
    if (actorRunId !== metadata.indexingRunId) {
      throw new BadRequestError("Event actorRunId does not match metadata");
    }

    return await this.getActorRunResult(knowledge, metadata);
  }

  public async loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult,
    index: number
  ): Promise<IndexKnowledgeResponse> {
    if (!result.blobUrl) {
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const response = await fetch(result.blobUrl);
    if (response.status !== 200) {
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }
    const data = await response.json();

    if (!data.items || !data.items[index]) {
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    let { documentCount, totalTokenCount } = await fileLoader.loadJsonArray(
      [data.items[index]],
      knowledge.id
    );

    logWithTimestamp(
      `Loaded file for knowledge ${knowledge.id} index ${index}`
    );

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
      blobUrl: result.blobUrl,
      metadata: {
        documentCount,
        totalTokenCount,
        completedChunks: [index],
      },
    };
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

    // return this.getActorRunResult(knowledge, metadata);
    //TODO: Re-implement
    return {
      indexStatus: KnowledgeIndexStatus.INDEXING,
    };
  }

  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    fileLoader.deleteKnowledge(knowledgeId);
  }

  private async getActorRunResult(
    knowledge: Knowledge,
    metadata: WebUrlMetadata
  ) {
    logWithTimestamp(
      `Retrieving actor run result for indexingRunId=${metadata.indexingRunId}`
    );
    const result = await apifyAdapter.getActorRunResult(metadata.indexingRunId);

    let blobUrl, chunkCount;
    if (
      result.items &&
      (result.status === KnowledgeIndexingResultStatus.PARTIAL ||
        result.status === KnowledgeIndexingResultStatus.SUCCESSFUL)
    ) {
      const cloudBlob = await put(
        `${knowledge.name}.json`,
        JSON.stringify(result),
        {
          access: "public",
        }
      );
      blobUrl = cloudBlob.url;
      chunkCount = result.items.length;
    }

    return {
      status: result.status,
      blobUrl,
      chunkCount,
    };
  }

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    return [];
  }
}

const webUrlsDataSourceAdapter = new WebUrlsDataSourceAdapter();
export default webUrlsDataSourceAdapter;
