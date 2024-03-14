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
import dataSourceViewingService from "@/src/domain/services/DataSourceViewingService";
import { inngest } from "./client";

const LIST_RESULTS_BATCH_SIZE = 100;

export enum ApifyEvent {
  APIFY_ACTOR_RUN_STARTED = "apify.actor.run.started",
  APIFY_WEBHOOK_RECEIVED = "apify.webhook.received",
}

export interface ApifyActorRunStartedPayload {
  actorRunId: string;
  dataSourceId: string;
  rootUrl: string;
}

export interface ApifyWebhookReceivedPayload {
  apifyEvent: ApifyWebhookEvent;
}

export const onApifyActorRunStarted = inngest.createFunction(
  { id: "on-apify-actor-run-started" },
  { event: ApifyEvent.APIFY_ACTOR_RUN_STARTED },
  async ({ event, step }) => {
    const { actorRunId, dataSourceId, rootUrl } =
      event.data as ApifyActorRunStartedPayload;

    while (true) {
      await step.sleep("sleep-for-1-minute", "1m");

      const actorRunResult = await pollActorRun(dataSourceId, actorRunId, step);

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
    const { dataSourceId, eventData } = apifyEvent;

    await pollActorRun(dataSourceId, eventData.actorRunId, step);
  }
);

const pollActorRun = async (
  dataSourceId: string,
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
      return await apifyAdapter.getActorRunBatch(
        actorRunId,
        rootUrl,
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

  return actorRunResult;
};
