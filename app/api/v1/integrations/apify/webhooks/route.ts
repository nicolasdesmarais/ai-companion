import {
  ApifyEvent,
  ApifyWebhookReceivedPayload,
} from "@/src/adapter-in/inngest/apify-workflows";
import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import {
  ApifySupportedEvents,
  ApifyWebhookEvent,
} from "@/src/domain/models/ApifyWebhookEvent";
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

  const apifyWebhookReceivedEventPayload: ApifyWebhookReceivedPayload = {
    apifyEvent: event,
  };
  await publishEvent(
    ApifyEvent.APIFY_WEBHOOK_RECEIVED,
    apifyWebhookReceivedEventPayload
  );

  return new Response("", { status: 200 });
}
