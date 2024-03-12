import { RetrieveContentResponseStatus } from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import webUrlsDataSourceAdapter from "@/src/adapter-out/knowledge/web-urls/WebUrlsCrawlerDataSourceAdapter";
import {
  DomainEvent,
  KnowledgeContentReceivedPayload,
} from "@/src/domain/events/domain-event";
import { ApifyWebhookEvent } from "@/src/domain/models/ApifyWebhookEvent";
import knowledgeService from "@/src/domain/services/KnowledgeService";
import { inngest } from "./client";

export enum ApifyEvent {
  APIFY_WEBHOOK_RECEIVED = "apify.webhook.received",
}

export interface ApifyWebhookReceivedPayload {
  apifyEvent: ApifyWebhookEvent;
}

export const onApifyWebhookReceived = inngest.createFunction(
  { id: "on-apify-webhook-received" },
  { event: ApifyEvent.APIFY_WEBHOOK_RECEIVED },
  async ({ event, step }) => {
    const { apifyEvent } = event.data as ApifyWebhookReceivedPayload;
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
      retrieveContentResponse.status ===
        RetrieveContentResponseStatus.SUCCESS &&
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
  }
);
