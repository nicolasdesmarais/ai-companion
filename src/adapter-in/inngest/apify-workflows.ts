import { RetrieveContentResponseStatus } from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import webUrlsDataSourceAdapter from "@/src/adapter-out/knowledge/web-urls/WebUrlsDataSourceAdapter";
import { DomainEvent } from "@/src/domain/events/domain-event";
import { ApifyWebhookEvent } from "@/src/domain/models/ApifyWebhookEvent";
import knowledgeService from "@/src/domain/services/KnowledgeService";
import { inngest } from "./client";

export enum ApifyEvent {
  APIFY_WEBHOOK_RECEIVED = "apify.webhook.received",
}

export interface ApifyWebhookReceivedPayload {
  apifyEvent: ApifyWebhookEvent;
}

export const apifyWebhookReceived = inngest.createFunction(
  { id: "apify-webhook-received" },
  { event: ApifyEvent.APIFY_WEBHOOK_RECEIVED },
  async ({ event, step }) => {
    const eventPayload = event.data as ApifyWebhookReceivedPayload;
    const { apifyEvent } = eventPayload;

    const { knowledgeId } = apifyEvent;
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

    if (
      retrieveContentResponse.status === RetrieveContentResponseStatus.SUCCESS
    ) {
      await step.sendEvent("knowledge-content-received-event", {
        name: DomainEvent.KNOWLEDGE_CONTENT_RETRIEVED,
        data: eventPayload,
      });
    }
  }
);
