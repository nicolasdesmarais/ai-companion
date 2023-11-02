import { DataSourceType } from "@prisma/client";
import dataSourceService from "../../domain/services/DataSourceService";
import { inngest } from "./client";

export const apifyWebhookReceived = inngest.createFunction(
  { id: "apify-webhooks" },
  { event: "apify/webhook.received" },
  async ({ event, step }) => {
    await step.run("handle-apify-webhook", async () => {
      dataSourceService.handleKnowledgeIndexedEvent(
        DataSourceType.WEB_URL,
        event.data.apifyEvent
      );
    });
  }
);
