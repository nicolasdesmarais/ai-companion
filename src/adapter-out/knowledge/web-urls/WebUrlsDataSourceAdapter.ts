import { BadRequestError } from "@/src/domain/errors/Errors";
import { ApifyWebhookEvent } from "@/src/domain/models/ApifyWebhookEvent";
import { KnowledgeDto } from "@/src/domain/models/DataSources";
import { logWithTimestamp } from "@/src/lib/logging";
import { Knowledge, KnowledgeIndexStatus } from "@prisma/client";
import { put } from "@vercel/blob";
import fileLoader from "../knowledgeLoaders/FileLoader";
import {
  ContentRetrievingDataSourceAdapter,
  DataSourceAdapter,
} from "../types/DataSourceAdapter";
import {
  DataSourceItem,
  DataSourceItemList,
  RetrieveContentAdapterResponse,
  RetrieveContentResponseStatus,
} from "../types/DataSourceTypes";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import {
  KnowledgeIndexingResult,
  KnowledgeIndexingResultStatus,
} from "../types/KnowlegeIndexingResult";
import apifyAdapter from "./ApifyAdapter";
import { WebUrlDataSourceInput } from "./types/WebUrlDataSourceInput";
import { WebUrlMetadata } from "./types/WebUrlMetadata";

export class WebUrlsDataSourceAdapter
  implements DataSourceAdapter, ContentRetrievingDataSourceAdapter
{
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as WebUrlDataSourceInput;
    const result: DataSourceItemList = {
      items: [
        {
          name: input.url,
          uniqueId: input.url,
        },
      ],
    };
    return result;
  }

  public async retrieveKnowledgeContent(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<RetrieveContentAdapterResponse> {
    const input = data as WebUrlDataSourceInput;
    const actorRunId = await apifyAdapter.startUrlIndexing(
      orgId,
      knowledge.id,
      input.url
    );

    if (!actorRunId) {
      return {
        status: RetrieveContentResponseStatus.FAILED,
      };
    }

    const metadata: WebUrlMetadata = {
      indexingRunId: actorRunId,
    };
    return {
      status: RetrieveContentResponseStatus.PENDING,
      metadata,
    };
  }

  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    const input = data as WebUrlDataSourceInput;
    const actorRunId = await apifyAdapter.startUrlIndexing(
      orgId,
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

  public async retrieveContentFromEvent(
    knowledge: KnowledgeDto,
    data: ApifyWebhookEvent
  ): Promise<RetrieveContentAdapterResponse> {
    const { actorRunId } = data.eventData;
    const metadata = knowledge.metadata as unknown as WebUrlMetadata;
    if (actorRunId !== metadata.indexingRunId) {
      throw new BadRequestError("Event actorRunId does not match metadata");
    }

    const result = await apifyAdapter.getActorRunResult(metadata.indexingRunId);

    let status: RetrieveContentResponseStatus;
    let originalContent;
    if (
      result.items &&
      (result.status === KnowledgeIndexingResultStatus.PARTIAL ||
        result.status === KnowledgeIndexingResultStatus.SUCCESSFUL)
    ) {
      const filename = `${knowledge.name}.json`;
      const cloudBlob = await put(filename, JSON.stringify(result), {
        access: "public",
      });
      status = RetrieveContentResponseStatus.SUCCESS;

      originalContent = {
        contentBlobUrl: cloudBlob.url,
        filename,
        mimeType: "application/json",
      };
    } else {
      status = RetrieveContentResponseStatus.FAILED;
    }

    return {
      status,
      originalContent,
    };
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

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    return [];
  }
}

const webUrlsDataSourceAdapter = new WebUrlsDataSourceAdapter();
export default webUrlsDataSourceAdapter;
