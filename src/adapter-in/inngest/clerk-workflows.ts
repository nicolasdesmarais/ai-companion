import aiService from "@/src/domain/services/AIService";
import groupService from "@/src/domain/services/GroupService";
import orgSubscriptionService from "@/src/domain/services/OrgSubsriptionService";
import {
  OrganizationWebhookEvent,
  UserJSON,
  UserWebhookEvent,
  WebhookEvent,
} from "@clerk/nextjs/server";
import { inngest } from "./client";

export enum ClerkEvent {
  CLERK_WEBHOOK_RECEIVED = "clerk.webhook.received",
}

enum ClerkWebhookEventType {
  USER_CREATED = "user.created",
  ORGANIZATION_CREATED = "organization.created",
}

export interface ClerkWebhookReceivedPayload {
  clerkEvent: WebhookEvent;
}

export const clerkWebhookReceived = inngest.createFunction(
  { id: "clerk-webhook-received" },
  { event: ClerkEvent.CLERK_WEBHOOK_RECEIVED },
  async ({ event, step }) => {
    const eventPayload = event.data as ClerkWebhookReceivedPayload;
    const { clerkEvent } = eventPayload;

    switch (clerkEvent.type) {
      case ClerkWebhookEventType.USER_CREATED:
        await handleUserCreatedEvent(step, clerkEvent);
        break;
      case ClerkWebhookEventType.ORGANIZATION_CREATED:
        await handleOrgCreatedEvent(step, clerkEvent);
        break;
      default:
        console.warn(`Unsupported event type: ${clerkEvent.type}`);
    }
  }
);

const handleUserCreatedEvent = async (step: any, clerkEvent: WebhookEvent) => {
  const userEvent = clerkEvent as UserWebhookEvent;
  const data = userEvent.data;
  const primaryEmail = getPrimaryEmailFromUserJson(data as UserJSON);
  if (primaryEmail === null) {
    console.log(
      "Cannot extract primary email from user data: " + JSON.stringify(data)
    );
    return;
  }

  const userId = data.id;
  if (!userId) {
    console.log("Cannot extract id from user data: " + JSON.stringify(data));
    return;
  }

  await step.run("populate-group-user-id", async () => {
    return await groupService.populateGroupUserId(userId, primaryEmail);
  });

  await step.run("populate-user-ai-permissions", async () => {
    return await aiService.populateAiPermissionsUserId(userId, primaryEmail);
  });
};

const getPrimaryEmailFromUserJson = (data: UserJSON): string | null => {
  const primaryEmailId = data.primary_email_address_id;
  const primaryEmail = data.email_addresses.find(
    (emailAddress: any) => emailAddress.id === primaryEmailId
  );
  return primaryEmail ? primaryEmail.email_address : null;
};

const handleOrgCreatedEvent = async (step: any, clerkEvent: WebhookEvent) => {
  const orgEvent = clerkEvent as OrganizationWebhookEvent;
  const orgId = orgEvent.data.id;
  if (!orgId) {
    return;
  }

  await step.run("create-initial-org-subscription", async () => {
    return await orgSubscriptionService.createInitialOrgSubscription(orgId);
  });
};
