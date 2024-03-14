import {
  ApifyActorRunStartedPayload,
  ApifyEvent,
} from "@/src/adapter-in/inngest/apify-workflows";
import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import { BadRequestError } from "@/src/domain/errors/Errors";
import { ApifyWebhookEvent } from "@/src/domain/models/ApifyWebhookEvent";
import { KnowledgeDto } from "@/src/domain/models/DataSources";
import { FileStorageService } from "@/src/domain/services/FileStorageService";
import knowledgeService from "@/src/domain/services/KnowledgeService";
import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
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
import { KnowledgeIndexingResultStatus } from "../types/KnowlegeIndexingResult";
import apifyAdapter from "./ApifyAdapter";
import {
  WebUrlDataSourceData,
  WebUrlDataSourceInput,
} from "./types/WebUrlDataSourceInput";
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
    const url = new URL(input.url).href;

    // Check if URL has already been indexed
    // If it does, return the existing knowledge, along with all knowledge for child URLs
    const existingKnowledge =
      await knowledgeService.findKnowledgeByTypeAndParent(
        DataSourceType.WEB_URL,
        url
      );

    if (existingKnowledge.length > 0) {
      const items: DataSourceItem[] = existingKnowledge.map((knowledge) => {
        return {
          name: knowledge.name,
          uniqueId: knowledge.uniqueId ?? undefined,
          parentUniqueId: knowledge.parentUniqueId ?? undefined,
          originalContent: knowledge.originalContent ?? undefined,
        };
      });

      return {
        items,
      };
    }

    // If URL has not been indexed, start indexing
    const actorRunId = await apifyAdapter.startUrlIndexing(
      orgId,
      dataSourceId,
      url
    );
    if (!actorRunId) {
      throw new Error("Failed to start actor run");
    }

    const runStartedEventPayload: ApifyActorRunStartedPayload = {
      actorRunId,
      dataSourceId,
      rootUrl: url,
    };
    await publishEvent(
      ApifyEvent.APIFY_ACTOR_RUN_STARTED,
      runStartedEventPayload
    );

    const dataSourceData: WebUrlDataSourceData = {
      indexingRunId: actorRunId,
      ...input,
    };

    return { data: dataSourceData, items: [] };
  }

  public async retrieveKnowledgeContent(
    orgId: string,
    userId: string,
    dataSourceId: string,
    knowledge: KnowledgeDto,
    data: any
  ): Promise<RetrieveContentAdapterResponse> {
    // Method not required for web URLs
    throw new Error("Method not implemented.");
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    if (knowledge.uniqueId !== item.uniqueId) {
      return true;
    }

    if (knowledge.uniqueId !== knowledge.parentUniqueId) {
      // Don't re-index child URLs
      return false;
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
      const contentBlobUrl = await FileStorageService.put(
        filename,
        JSON.stringify(result)
      );
      status = RetrieveContentResponseStatus.SUCCESS;

      originalContent = {
        contentBlobUrl,
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

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    return [];
  }
}

const webUrlsDataSourceAdapter = new WebUrlsDataSourceAdapter();
export default webUrlsDataSourceAdapter;
