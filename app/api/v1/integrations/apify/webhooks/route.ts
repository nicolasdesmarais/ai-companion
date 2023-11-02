import { inngest } from "@/src/adapters/inngest/client";
import {
  ApifySupportedEvents,
  ApifyWebhookEvent,
} from "@/src/domain/types/ApifyWebhookEvent";
import { headers } from "next/headers";

const isSupportedEvent = (
  eventType: string
): eventType is ApifySupportedEvents => {
  return Object.values(ApifySupportedEvents).includes(
    eventType as ApifySupportedEvents
  );
};
export const maxDuration = 300;
const webhookSecret = process.env.APIFY_WEBHOOK_SECRET;

export async function POST(req: Request) {
  console.log("Received Apify Webhook");
  const headerPayload = headers();

  const secret = headerPayload.get("X-Apify-Webhook-Secret");
  if (secret !== webhookSecret) {
    console.log("Invalid Apify Webhook secret");
    return new Response("", { status: 400 });
  }

  const payload = await req.json();
  const event = payload as ApifyWebhookEvent;
  console.log("Apify Webhook payload: " + JSON.stringify(event));
  if (!isSupportedEvent(event.eventType)) {
    console.log(`Unsupported event type: ${event.eventType}`);
    return new Response("", { status: 200 });
  }

  await inngest.send({
    name: "apify/webhook.received",
    data: {
      apifyEvent: event,
    },
  });

  return new Response("", { status: 200 });
}
