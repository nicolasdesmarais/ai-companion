import {
  ApifyActorRunRequestedPayload,
  ApifyEvent,
} from "@/src/adapter-in/inngest/apify-workflows";
import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import { KnowledgeDto } from "@/src/domain/models/DataSources";
import { Knowledge, KnowledgeIndexStatus } from "@prisma/client";
import {
  ContentRetrievingDataSourceAdapter,
  DataSourceAdapter,
  ShouldReindexKnowledgeResponse,
} from "../types/DataSourceAdapter";
import {
  DataSourceItem,
  DataSourceItemList,
  RetrieveContentAdapterResponse,
  RetrieveContentResponseStatus,
} from "../types/DataSourceTypes";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { WebUrlDataSourceInput } from "./types/WebUrlDataSourceInput";
import { WebUrlMetadata } from "./types/WebUrlMetadata";

export class WebUrlsWebsiteContentCrawlerAdapter
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
          parentUniqueId: input.url,
        },
      ],
    };
    return result;
  }

  public async retrieveKnowledgeContent(
    orgId: string,
    userId: string,
    dataSourceId: string,
    knowledge: KnowledgeDto,
    data: any
  ): Promise<RetrieveContentAdapterResponse> {
    const actorRunRequestedPayload: ApifyActorRunRequestedPayload = {
      orgId,
      dataSourceId,
      knowledgeId: knowledge.id,
      url: knowledge.name,
    };
    await publishEvent(
      ApifyEvent.APIFY_ACTOR_RUN_REQUESTED,
      actorRunRequestedPayload
    );

    return {
      status: RetrieveContentResponseStatus.PENDING,
    };
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): ShouldReindexKnowledgeResponse {
    if (knowledge.uniqueId !== item.uniqueId) {
      return { shouldReindex: true };
    }

    if (
      (knowledge.metadata as unknown as WebUrlMetadata)?.indexingRunId ===
      item.metadata?.indexingRunId
    ) {
      return { shouldReindex: false, includeChildren: false };
    }

    if (item.uniqueId !== item.parentUniqueId) {
      // Reindex if the item is not a root item
      return { shouldReindex: true };
    }

    if (knowledge.indexStatus !== KnowledgeIndexStatus.COMPLETED) {
      return { shouldReindex: true };
    }

    const oneWeekAgo = new Date();
    oneWeekAgo.setDate(oneWeekAgo.getDate() - 7);
    if (!knowledge.lastIndexedAt || knowledge.lastIndexedAt < oneWeekAgo) {
      return { shouldReindex: true };
    } else {
      return { shouldReindex: false, includeChildren: true };
    }
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

const webUrlsWebsiteContentCrawlerAdapter =
  new WebUrlsWebsiteContentCrawlerAdapter();
export default webUrlsWebsiteContentCrawlerAdapter;
