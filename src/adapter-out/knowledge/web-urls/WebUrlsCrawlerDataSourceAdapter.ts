import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import {
  DataSourceItemListReceivedPayload,
  DomainEvent,
} from "@/src/domain/events/domain-event";
import { KnowledgeDto } from "@/src/domain/models/DataSources";
import { FileStorageService } from "@/src/domain/services/FileStorageService";
import { Knowledge } from "@prisma/client";
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
import crawler from "./crawler";
import { WebUrlDataSourceInput } from "./types/WebUrlDataSourceInput";

const maxDepth = process.env.WEB_URL_MAX_DEPTH;

export interface WebUrlCrawlerMetadata {
  requestId: string;
  rootUrl: string;
  depth: number;
}

export class WebUrlsCrawlerDataSourceAdapter
  implements DataSourceAdapter, ContentRetrievingDataSourceAdapter
{
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as WebUrlDataSourceInput;
    let requestId = Math.random().toString(36).substring(2, 15);

    const result: DataSourceItemList = {
      items: [
        {
          name: input.url,
          uniqueId: input.url,
          metadata: {
            requestId,
            rootUrl: input.url,
            depth: 0,
          } as WebUrlCrawlerMetadata,
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
    const { url: rootUrl } = input;

    const url = knowledge.name;
    const depth = (knowledge.metadata as WebUrlCrawlerMetadata).depth ?? 0;
    const requestId = (knowledge.metadata as WebUrlCrawlerMetadata).requestId;

    let extractChildUrls = true;
    if (maxDepth && depth >= parseInt(maxDepth)) {
      extractChildUrls = false;
    }

    const page = await crawler.crawl(url, extractChildUrls);

    const filename = `${url}.md`;
    const contentBlobUrl = await FileStorageService.put(filename, page.content);

    for (const childUrl of page.childUrls) {
      const dataSourceItemList: DataSourceItemList = {
        items: [
          {
            name: childUrl,
            uniqueId: childUrl,
            metadata: {
              requestId,
              rootUrl,
              depth: depth + 1,
            } as WebUrlCrawlerMetadata,
          },
        ],
      };

      const eventPayload: DataSourceItemListReceivedPayload = {
        dataSourceId,
        dataSourceItemList,
        forRefresh: false,
        forceRefresh: false,
      };

      await publishEvent(
        DomainEvent.DATASOURCE_ITEM_LIST_RECEIVED,
        eventPayload
      );
    }

    return {
      status: RetrieveContentResponseStatus.SUCCESS,
      originalContent: {
        contentBlobUrl,
        filename,
        mimeType: "text/markdown",
      },
    };
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    if (knowledge.uniqueId !== item.uniqueId) {
      return true;
    }

    const knowledgeRequestId = (
      knowledge.metadata as unknown as WebUrlCrawlerMetadata
    ).requestId;
    const itemRequestId = (item.metadata as WebUrlCrawlerMetadata).requestId;
    if (knowledgeRequestId === itemRequestId) {
      // Avoid re-indexing the same URL if it's part of the same request
      return false;
    }

    const oneWeekAgo = new Date();
    oneWeekAgo.setDate(oneWeekAgo.getDate() - 7);
    return !knowledge.lastIndexedAt || knowledge.lastIndexedAt < oneWeekAgo;
  }

  public async pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    return [];
  }
}

const webUrlsCrawlerDataSourceAdapter = new WebUrlsCrawlerDataSourceAdapter();
export default webUrlsCrawlerDataSourceAdapter;
