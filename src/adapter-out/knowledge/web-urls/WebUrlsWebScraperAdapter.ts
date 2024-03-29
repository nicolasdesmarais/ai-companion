import { BadRequestError } from "@/src/domain/errors/Errors";
import { ApifyWebhookEvent } from "@/src/domain/models/ApifyWebhookEvent";
import { KnowledgeDto } from "@/src/domain/models/DataSources";
import { FileStorageService } from "@/src/domain/services/FileStorageService";
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
import { KnowledgeIndexingResultStatus } from "../types/KnowlegeIndexingResult";
import apifyWebScraper from "./ApifyWebScraper";
import { WebUrlDataSourceInput } from "./types/WebUrlDataSourceInput";
import { WebUrlMetadata } from "./types/WebUrlMetadata";

export class WebUrlsWebScraperAdapter
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
    dataSourceId: string,
    knowledge: KnowledgeDto,
    data: any
  ): Promise<RetrieveContentAdapterResponse> {
    const input = data as WebUrlDataSourceInput;
    const actorRunId = await apifyWebScraper.startUrlIndexing(
      orgId,
      dataSourceId,
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

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): ShouldReindexKnowledgeResponse {
    if (knowledge.uniqueId !== item.uniqueId) {
      return { shouldReindex: true };
    }

    if (knowledge.indexStatus !== KnowledgeIndexStatus.COMPLETED) {
      return { shouldReindex: true };
    }

    const oneWeekAgo = new Date();
    oneWeekAgo.setDate(oneWeekAgo.getDate() - 7);
    const shouldReindex =
      !knowledge.lastIndexedAt || knowledge.lastIndexedAt < oneWeekAgo;
    return { shouldReindex };
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

    const result = await apifyWebScraper.getActorRunResult(
      metadata.indexingRunId
    );

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

const webUrlsWebScraperAdapter = new WebUrlsWebScraperAdapter();
export default webUrlsWebScraperAdapter;
