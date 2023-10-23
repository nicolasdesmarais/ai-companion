import { headers } from "next/headers";

enum ApifySupportedEvents {
  ACTOR_RUN_SUCCEEDED = "ACTOR.RUN.SUCCEEDED",
  ACTOR_RUN_FAILED = "ACTOR.RUN.FAILED",
  ACTOR_RUN_ABORTED = "ACTOR.RUN.ABORTED",
  ACTOR_RUN_TIMED_OUT = "ACTOR.RUN.TIMED_OUT",
}

const isSupportedEvent = (
  eventType: string
): eventType is ApifySupportedEvents => {
  return Object.values(ApifySupportedEvents).includes(
    eventType as ApifySupportedEvents
  );
};

interface ApifyWebhookEvent {
  eventType: string;
  eventData: ApifyEventData;
}

interface ApifyEventData {
  actorId: string;
  actorRunId: string;
}

export async function POST(req: Request) {
  console.log("APIFY Webhook:");
  const headerPayload = headers();
  const header = JSON.stringify(headerPayload);
  console.log("Headers:" + header);

  const payload = await req.json();
  const event = payload as ApifyWebhookEvent;
  console.log(event);
  if (!isSupportedEvent(event.eventType)) {
    console.log(`Unsupported event type: ${event.eventType}`);
    return new Response("", { status: 200 });
  }

  return new Response("", { status: 200 });
}
