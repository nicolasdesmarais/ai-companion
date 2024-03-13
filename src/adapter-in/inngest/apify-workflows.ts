import apifyAdapter, {
  ActorRunResult,
  ActorRunStatus,
} from "@/src/adapter-out/knowledge/web-urls/ApifyAdapter";
import {
  DataSourceItemListReceivedPayload,
  DomainEvent,
} from "@/src/domain/events/domain-event";
import { ApifyWebhookEvent } from "@/src/domain/models/ApifyWebhookEvent";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import { inngest } from "./client";

const LIST_RESULTS_BATCH_SIZE = 100;

export enum ApifyEvent {
  APIFY_ACTOR_RUN_STARTED = "apify.actor.run.started",
  APIFY_WEBHOOK_RECEIVED = "apify.webhook.received",
}

export interface ApifyActorRunStartedPayload {
  actorRunId: string;
  dataSourceId: string;
}

export interface ApifyWebhookReceivedPayload {
  apifyEvent: ApifyWebhookEvent;
}

export const onApifyActorRunStarted = inngest.createFunction(
  { id: "on-apify-actor-run-started" },
  { event: ApifyEvent.APIFY_ACTOR_RUN_STARTED },
  async ({ event, step }) => {
    const { actorRunId, dataSourceId } =
      event.data as ApifyActorRunStartedPayload;

    let offset = 0;
    while (true) {
      await step.waitForEvent("wait-for-apify-webhook", {
        event: ApifyEvent.APIFY_WEBHOOK_RECEIVED,
        timeout: "1m",
        match: "data.eventData.actorRunId",
      });

      let batchResults = 0;
      let actorRunResult: ActorRunResult;
      do {
        actorRunResult = await step.run("get-actor-run-batch", async () => {
          return await apifyAdapter.getActorRunBatch(
            actorRunId,
            offset,
            LIST_RESULTS_BATCH_SIZE
          );
        });

        batchResults = actorRunResult.items.length;
        if (batchResults > 0) {
          offset += actorRunResult.items.length;

          const eventPayload: DataSourceItemListReceivedPayload = {
            dataSourceId,
            dataSourceItemList: { items: actorRunResult.items },
            forRefresh: false,
            forceRefresh: false,
          };
          await step.sendEvent("datasource-item-list-received", {
            name: DomainEvent.DATASOURCE_ITEM_LIST_RECEIVED,
            data: eventPayload,
          });

          await step.run("update-data-source", async () => {
            return await dataSourceManagementService.updateDataSourceData(
              dataSourceId,
              { offset }
            );
          });
        }
      } while (batchResults >= LIST_RESULTS_BATCH_SIZE);

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
    // const { apifyEvent } = event.data as ApifyWebhookReceivedPayload;
    // const { dataSourceId, knowledgeId } = apifyEvent;
    // const knowledge = await step.run("fetch-knowledge", async () => {
    //   return await knowledgeService.getKnowledge(knowledgeId);
    // });
    // const retrieveContentResponse = await step.run(
    //   "retrieve-content-from-event",
    //   async () => {
    //     return await webUrlsDataSourceAdapter.retrieveContentFromEvent(
    //       knowledge,
    //       apifyEvent
    //     );
    //   }
    // );
    // const { originalContent } = retrieveContentResponse;
    // if (
    //   retrieveContentResponse.status ===
    //     RetrieveContentResponseStatus.SUCCESS &&
    //   originalContent
    // ) {
    //   const knowledgeContentRetrievedEventPayload: KnowledgeContentReceivedPayload =
    //     {
    //       dataSourceId,
    //       knowledgeId,
    //       originalContent,
    //     };
    //   await step.sendEvent("knowledge-content-received-event", {
    //     name: DomainEvent.KNOWLEDGE_CONTENT_RETRIEVED,
    //     data: knowledgeContentRetrievedEventPayload,
    //   });
    // }
  }
);
