import {
  DataSourceItem,
  RetrieveContentResponseStatus,
} from "@/src/adapter-out/knowledge/types/DataSourceTypes";

import apifyWebsiteContentCrawler, {
  ActorRunItem,
  ActorRunResult,
  ActorRunStatus,
} from "@/src/adapter-out/knowledge/web-urls/ApifyWebsiteContentCrawler";
import webUrlsWebScraperAdapter from "@/src/adapter-out/knowledge/web-urls/WebUrlsWebScraperAdapter";
import { WebUrlMetadata } from "@/src/adapter-out/knowledge/web-urls/types/WebUrlMetadata";
import {
  DataSourceItemListReceivedPayload,
  DomainEvent,
  KnowledgeContentReceivedPayload,
} from "@/src/domain/events/domain-event";
import { ApifyWebhookEvent } from "@/src/domain/models/ApifyWebhookEvent";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import dataSourceViewingService from "@/src/domain/services/DataSourceViewingService";
import knowledgeService from "@/src/domain/services/KnowledgeService";
import { inngest } from "./client";

const LIST_RESULTS_BATCH_SIZE = 10;
const useCheerioAdapter = process.env.USE_CHEERIO_ADAPTER === "true";

export enum ApifyEvent {
  APIFY_ACTOR_RUN_REQUESTED = "apify.actor.run.requested",
  APIFY_WEBHOOK_RECEIVED = "apify.webhook.received",
}

export interface ApifyActorRunRequestedPayload {
  orgId: string;
  dataSourceId: string;
  knowledgeId: string;
  url: string;
}

export interface ApifyWebhookReceivedPayload {
  apifyEvent: ApifyWebhookEvent;
}

export const onApifyActorRunRequested = inngest.createFunction(
  {
    id: "on-apify-actor-run-requested",
    concurrency: {
      limit: 12,
    },
  },
  { event: ApifyEvent.APIFY_ACTOR_RUN_REQUESTED },
  async ({ event, step }) => {
    const { orgId, dataSourceId, knowledgeId, url } =
      event.data as ApifyActorRunRequestedPayload;

    const actorRunId = await step.run("start-url-indexing", async () => {
      return await apifyWebsiteContentCrawler.startUrlIndexing(
        orgId,
        dataSourceId,
        knowledgeId,
        url
      );
    });

    if (!actorRunId) {
      throw new Error("Failed to start actor run");
    }

    let rootItems: ActorRunItem[] = [];
    let iteration = 0;
    while (true) {
      const webhookReceived = await step.waitForEvent(
        `wait-for-apify-webhook-${iteration}`,
        {
          event: ApifyEvent.APIFY_WEBHOOK_RECEIVED,
          timeout: "1m",
          if: `async.data.apifyEvent.eventData.actorRunId == '${actorRunId}'`,
        }
      );

      if (webhookReceived) {
        // Stop polling when webhook is received
        break;
      }

      // No webhook received yet, continue polling
      const { actorRunResult, rootItems: batchRootItems } = await pollActorRun(
        dataSourceId,
        actorRunId,
        step,
        iteration
      );
      rootItems.push(...batchRootItems);

      if (actorRunResult.status !== ActorRunStatus.INDEXING) {
        // We've reached a terminal state
        break;
      }

      iteration++;
    }

    for (const item of rootItems) {
      await publishRootUrlEvent(dataSourceId, knowledgeId, item, step);
    }
  }
);

export const onApifyWebhookReceived = inngest.createFunction(
  { id: "on-apify-webhook-received" },
  { event: ApifyEvent.APIFY_WEBHOOK_RECEIVED },
  async ({ event, step }) => {
    const { apifyEvent } = event.data as ApifyWebhookReceivedPayload;
    const { dataSourceId, knowledgeId, eventData } = apifyEvent;

    if (useCheerioAdapter) {
      const { rootItems } = await pollActorRun(
        dataSourceId,
        eventData.actorRunId,
        step
      );

      for (const item of rootItems) {
        await publishRootUrlEvent(dataSourceId, knowledgeId, item, step);
      }
    } else {
      await processWebScraperWebhook(apifyEvent, step);
    }
  }
);

const pollActorRun = async (
  dataSourceId: string,
  actorRunId: string,
  step: any,
  iteration?: number
) => {
  const dataSource = await dataSourceViewingService.getById(dataSourceId);

  const rootUrl = dataSource.name;
  let offset = dataSource.data.offset || 0;
  let batchResults = 0;
  let actorRunResult: ActorRunResult;
  const rootItems: ActorRunItem[] = [];
  do {
    actorRunResult = await step.run(
      `get-actor-run-batch-${iteration}`,
      async () => {
        return await apifyWebsiteContentCrawler.getActorRunBatch(
          actorRunId,
          offset,
          LIST_RESULTS_BATCH_SIZE
        );
      }
    );

    batchResults = actorRunResult.items.length;
    if (batchResults > 0) {
      offset += actorRunResult.items.length;

      const dataSourceItems: DataSourceItem[] = [];

      for (const item of actorRunResult.items) {
        if (item.url === rootUrl) {
          rootItems.push(item);
        } else {
          dataSourceItems.push(
            mapActorRunItemToDataSourceItem(actorRunId, rootUrl, item)
          );
        }
      }

      if (dataSourceItems.length > 0) {
        const eventPayload: DataSourceItemListReceivedPayload = {
          dataSourceId,
          dataSourceItemList: { items: dataSourceItems },
          forRefresh: false,
          forceRefresh: false,
        };
        await step.sendEvent("datasource-item-list-received", {
          name: DomainEvent.DATASOURCE_ITEM_LIST_RECEIVED,
          data: eventPayload,
        });
      }
    }

    await step.run("update-data-source", async () => {
      return await dataSourceManagementService.updateDataSourceData(
        dataSourceId,
        { indexingRunId: actorRunId, offset }
      );
    });
  } while (batchResults >= LIST_RESULTS_BATCH_SIZE);

  return { actorRunResult, rootItems };
};

const publishRootUrlEvent = async (
  dataSourceId: string,
  knowledgeId: string,
  item: ActorRunItem,
  step: any
) => {
  const contentReceivedPayload: KnowledgeContentReceivedPayload = {
    dataSourceId,
    knowledgeId,
    originalContent: {
      contentBlobUrl: item.contentBlobUrl,
      filename: item.filename,
      mimeType: item.mimeType,
    },
  };

  await step.sendEvent("root-url-content-received", {
    name: DomainEvent.KNOWLEDGE_CONTENT_RETRIEVED,
    data: contentReceivedPayload,
  });
};

const mapActorRunItemToDataSourceItem = (
  actorRunId: string,
  rootUrl: string,
  actorRunItem: ActorRunItem
) => {
  const metadata: WebUrlMetadata = {
    indexingRunId: actorRunId,
  };
  return {
    name: actorRunItem.url,
    uniqueId: actorRunItem.url,
    parentUniqueId: rootUrl,
    originalContent: {
      contentBlobUrl: actorRunItem.contentBlobUrl,
      filename: actorRunItem.filename,
      mimeType: actorRunItem.mimeType,
    },
    metadata,
  };
};

const processWebScraperWebhook = async (
  apifyEvent: ApifyWebhookEvent,
  step: any
) => {
  const { dataSourceId, knowledgeId } = apifyEvent;

  const knowledge = await step.run("fetch-knowledge", async () => {
    return await knowledgeService.getKnowledge(knowledgeId);
  });

  const retrieveContentResponse = await step.run(
    "retrieve-content-from-event",
    async () => {
      return await webUrlsWebScraperAdapter.retrieveContentFromEvent(
        knowledge,
        apifyEvent
      );
    }
  );

  const { originalContent } = retrieveContentResponse;
  if (
    retrieveContentResponse.status === RetrieveContentResponseStatus.SUCCESS &&
    originalContent
  ) {
    const knowledgeContentRetrievedEventPayload: KnowledgeContentReceivedPayload =
      {
        dataSourceId,
        knowledgeId,
        originalContent,
      };
    await step.sendEvent("knowledge-content-received-event", {
      name: DomainEvent.KNOWLEDGE_CONTENT_RETRIEVED,
      data: knowledgeContentRetrievedEventPayload,
    });
  }
};
