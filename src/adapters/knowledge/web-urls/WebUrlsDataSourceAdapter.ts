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

    const result = await apifyAdapter.getActorRunResult(actorRunId);
    fileLoader.loadJsonArray(result, knowledge.id);
    return {
      indexStatus: KnowledgeIndexStatus.COMPLETED,
    };
  }
}

const webUrlsDataSourceAdapter = new WebUrlsDataSourceAdapter();
export default webUrlsDataSourceAdapter;
