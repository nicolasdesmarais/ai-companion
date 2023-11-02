import { apifyWebhookReceived } from "@/src/adapters/inngest/apifyWebhooks";
import { serve } from "inngest/next";
import { inngest } from "../../../src/adapters/inngest/client";

export const { GET, POST, PUT } = serve({
  client: inngest,
  functions: [apifyWebhookReceived],
});
