import aiService from "@/src/domain/services/AIService";
import groupService from "@/src/domain/services/GroupService";
import {
  SessionWebhookEvent,
  User,
  UserJSON,
  UserWebhookEvent,
  WebhookEvent,
} from "@clerk/nextjs/server";
import { headers } from "next/headers";
import { Webhook } from "svix";

enum SupportedEvents {
  USER_CREATED_EVENT = "user.created",
  SESSION_CREATED_EVENT = "session.created",
}

const isSupportedEvent = (eventType: string): eventType is SupportedEvents => {
  return Object.values(SupportedEvents).includes(eventType as SupportedEvents);
};

export async function POST(req: Request) {
  // You can find this in the Clerk Dashboard -> Webhooks -> choose the webhook
  const CLERK_WEBHOOK_SECRET = process.env.CLERK_WEBHOOK_SECRET;

  if (!CLERK_WEBHOOK_SECRET) {
    throw new Error(
      "Please add CLERK_WEBHOOK_SECRET from Clerk Dashboard to .env or .env.local"
    );
  }

  // Get the headers
  const headerPayload = headers();
  const svix_id = headerPayload.get("svix-id");
  const svix_timestamp = headerPayload.get("svix-timestamp");
  const svix_signature = headerPayload.get("svix-signature");

  // If there are no headers, error out
  if (!svix_id || !svix_timestamp || !svix_signature) {
    return new Response("Error occured -- no svix headers", {
      status: 400,
    });
  }

  // Get the body
  const payload = await req.json();
  const body = JSON.stringify(payload);

  // Create a new SVIX instance with your secret.
  const wh = new Webhook(CLERK_WEBHOOK_SECRET);

  let evt: WebhookEvent;

  // Verify the payload with the headers
  try {
    evt = wh.verify(body, {
      "svix-id": svix_id,
      "svix-timestamp": svix_timestamp,
      "svix-signature": svix_signature,
    }) as WebhookEvent;
  } catch (err) {
    console.error("Error verifying webhook:", err);
    return new Response("Error occured", {
      status: 400,
    });
  }

  if (!isSupportedEvent(evt.type)) {
    console.log(`Unsupported event type: ${evt.type}`);
    return;
  }

  const eventType: SupportedEvents = evt.type;
  switch (eventType) {
    case SupportedEvents.USER_CREATED_EVENT:
      await handleUserCreatedEvent(evt as UserWebhookEvent);
      break;
    case SupportedEvents.SESSION_CREATED_EVENT:
      await handleSessionCreatedEvent(evt as SessionWebhookEvent);
      break;
  }

  return new Response("", { status: 201 });
}

async function handleUserCreatedEvent(userEvent: UserWebhookEvent) {
  const data = userEvent.data;
  const primaryEmail = getPrimaryEmailFromUserJson(data as UserJSON);
  if (primaryEmail === null) {
    console.log(
      "Cannot extract primary email from user data: " + JSON.stringify(data)
    );
    return;
  }

  if (!data.id) {
    console.log("Cannot extract id from user data: " + JSON.stringify(data));
    return;
  }

  await groupService.populateGroupUserId(data.id, primaryEmail);
  await aiService.populateAiPermissionsUserId(data.id, primaryEmail);
}

async function handleSessionCreatedEvent(sessionEvent: SessionWebhookEvent) {
  // TODO: Remove this function if not needed
}

const getPrimaryEmailFromUserJson = (data: UserJSON): string | null => {
  const primaryEmailId = data.primary_email_address_id;
  const primaryEmail = data.email_addresses.find(
    (emailAddress: any) => emailAddress.id === primaryEmailId
  );
  return primaryEmail ? primaryEmail.email_address : null;
};

const getPrimaryEmailFromClerkUser = (clerkUser: User): string | null => {
  const primaryEmailId = clerkUser.primaryEmailAddressId;
  const primaryEmail = clerkUser.emailAddresses.find(
    (emailAddress: any) => emailAddress.id === primaryEmailId
  );
  return primaryEmail ? primaryEmail.emailAddress : null;
};
