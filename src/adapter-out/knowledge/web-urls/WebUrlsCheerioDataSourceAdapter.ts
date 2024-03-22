import {
  ApifyActorRunStartedPayload,
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
import apifyCheerioAdapter from "./ApifyCheerioAdapter";
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
    const url = knowledge.name;
    const knowledgeId = knowledge.id;
    const indexingRunId = (knowledge.metadata as WebUrlMetadata)?.indexingRunId;

    if (indexingRunId) {
      return await this.addUrlToExistingRun(
        indexingRunId,
        url,
        dataSourceId,
        knowledgeId
      );
    } else {
      return await this.startIndexingRun(orgId, dataSourceId, knowledgeId, url);
    }
  }

  private async addUrlToExistingRun(
    actorRunId: string,
    url: string,
    dataSourceId: string,
    knowledgeId: string
  ): Promise<RetrieveContentAdapterResponse> {
    const indexingRunResurrected =
      await apifyCheerioAdapter.addUrlToExistingRun(
        actorRunId,
        url,
        knowledgeId
      );

    if (indexingRunResurrected) {
      const runStartedEventPayload: ApifyActorRunStartedPayload = {
        actorRunId,
        dataSourceId,
        knowledgeId,
        rootUrl: url,
      };
      await publishEvent(
        ApifyEvent.APIFY_ACTOR_RUN_STARTED,
        runStartedEventPayload
      );
    }

    return {
      status: RetrieveContentResponseStatus.PENDING,
    };
  }

  private async startIndexingRun(
    orgId: string,
    dataSourceId: string,
    knowledgeId: string,
    url: string
  ): Promise<RetrieveContentAdapterResponse> {
    const actorRunId = await apifyCheerioAdapter.startUrlIndexing(
      orgId,
      dataSourceId,
      knowledgeId,
      url
    );

    if (!actorRunId) {
      return {
        status: RetrieveContentResponseStatus.FAILED,
      };
    }

    const runStartedEventPayload: ApifyActorRunStartedPayload = {
      actorRunId,
      dataSourceId,
      knowledgeId,
      rootUrl: url,
    };
    await publishEvent(
      ApifyEvent.APIFY_ACTOR_RUN_STARTED,
      runStartedEventPayload
    );

    const metadata: WebUrlMetadata = {
      indexingRunId: actorRunId,
    };
    return {
      status: RetrieveContentResponseStatus.PENDING,
      metadata,
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
      return { shouldReindex: false };
    }

    return { shouldReindex: true };
    //  TODO: Temporarily force reindexing, until we implement association of child URLs
    // if (knowledge.indexStatus !== KnowledgeIndexStatus.COMPLETED) {
    //   return true;
    // }

    // const oneWeekAgo = new Date();
    // oneWeekAgo.setDate(oneWeekAgo.getDate() - 7);
    // return !knowledge.lastIndexedAt || knowledge.lastIndexedAt < oneWeekAgo;
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

const webUrlsCheerioDataSourceAdapter = new WebUrlsDataSourceAdapter();
export default webUrlsCheerioDataSourceAdapter;
