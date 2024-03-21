import {
  DataSourceItem,
  RetrieveContentResponseStatus,
} from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import apifyCheerioAdapter, {
  ActorRunItem,
  ActorRunResult,
  ActorRunStatus,
} from "@/src/adapter-out/knowledge/web-urls/ApifyCheerioAdapter";
import webUrlsDataSourceAdapter from "@/src/adapter-out/knowledge/web-urls/WebUrlsDataSourceAdapter";
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
const MAX_EVENTS = 1000;
const useCheerioScraper = process.env.USE_CHEERIO_SCRAPER === "true";

export enum ApifyEvent {
  APIFY_ACTOR_RUN_STARTED = "apify.actor.run.started",
  APIFY_WEBHOOK_RECEIVED = "apify.webhook.received",
}

export interface ApifyActorRunStartedPayload {
  actorRunId: string;
  dataSourceId: string;
  knowledgeId: string;
  rootUrl: string;
}

export interface ApifyWebhookReceivedPayload {
  apifyEvent: ApifyWebhookEvent;
}

export const onApifyActorRunStarted = inngest.createFunction(
  { id: "on-apify-actor-run-started" },
  { event: ApifyEvent.APIFY_ACTOR_RUN_STARTED },
  async ({ event, step }) => {
    const { actorRunId, dataSourceId, knowledgeId } =
      event.data as ApifyActorRunStartedPayload;

    while (true) {
      await step.sleep("sleep-for-1-minute", "1m");

      const actorRunResult = await pollActorRun(
        dataSourceId,
        knowledgeId,
        actorRunId,
        step
      );

      if (actorRunResult.status !== ActorRunStatus.INDEXING) {
        // We've reached a terminal state
        break;
      }
    }
  }
);

export const onApifyWebhookReceived = inngest.createFunction(
  { id: "on-apify-webhook-received" },
  { event: ApifyEvent.APIFY_WEBHOOK_RECEIVED },
  async ({ event, step }) => {
    const { apifyEvent } = event.data as ApifyWebhookReceivedPayload;
    const { dataSourceId, knowledgeId, eventData } = apifyEvent;

    if (useCheerioScraper) {
      await pollActorRun(dataSourceId, knowledgeId, eventData.actorRunId, step);
    } else {
      await processWebScraperWebhook(apifyEvent, step);
    }
  }
);

const pollActorRun = async (
  dataSourceId: string,
  knowledgeId: string,
  actorRunId: string,
  step: any
) => {
  const dataSource = await dataSourceViewingService.getById(dataSourceId);

  const rootUrl = dataSource.name;
  let offset = dataSource.data.offset || 0;
  let batchResults = 0;
  let actorRunResult: ActorRunResult;
  do {
    actorRunResult = await step.run("get-actor-run-batch", async () => {
      return await apifyCheerioAdapter.getActorRunBatch(
        actorRunId,
        offset,
        LIST_RESULTS_BATCH_SIZE
      );
    });

    batchResults = actorRunResult.items.length;
    if (batchResults > 0) {
      offset += actorRunResult.items.length;

      let contentReceivedEvents = [];
      const childUrls: string[] = [];
      for (const item of actorRunResult.items) {
        contentReceivedEvents.push(
          createContentReceivedEvent(dataSourceId, knowledgeId, item, step)
        );

        if (contentReceivedEvents.length >= MAX_EVENTS) {
          await step.sendEvent(
            "publish-content-received-events",
            contentReceivedEvents
          );
          contentReceivedEvents = [];
        }

        childUrls.push(...item.childUrls);
      }

      if (childUrls.length > 0) {
        const dataSourceItems: DataSourceItem[] = childUrls.map((url) => {
          const metadata: WebUrlMetadata = {
            indexingRunId: actorRunId,
          };

          return {
            name: url,
            uniqueId: url,
            parentUniqueId: rootUrl,
            metadata,
          };
        });

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

      if (contentReceivedEvents.length > 0) {
        await step.sendEvent(
          "publish-content-received-events",
          contentReceivedEvents
        );
      }
    }

    await step.run("update-data-source", async () => {
      return await dataSourceManagementService.updateDataSourceData(
        dataSourceId,
        { offset }
      );
    });
  } while (batchResults >= LIST_RESULTS_BATCH_SIZE);

  return actorRunResult;
};

const createContentReceivedEvent = (
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

  return {
    name: DomainEvent.KNOWLEDGE_CONTENT_RETRIEVED,
    data: contentReceivedPayload,
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
      return await webUrlsDataSourceAdapter.retrieveContentFromEvent(
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
