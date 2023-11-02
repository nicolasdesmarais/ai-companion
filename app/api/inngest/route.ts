import { apifyWebhookReceived } from "@/src/adapters/inngest/apify/apify-webhooks";
import { googleDriveDataSourceCreationRequested } from "@/src/adapters/inngest/google-drive/google-drive-functions";
import { serve } from "inngest/next";
import { inngest } from "../../../src/adapters/inngest/client";

export const { GET, POST, PUT } = serve({
  client: inngest,
  functions: [apifyWebhookReceived, googleDriveDataSourceCreationRequested],
});
