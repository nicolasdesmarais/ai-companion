import stripeAdapter from "@/src/adapter-out/stripe/StripeAdapter";
import { ExternalOrgSubscription } from "@/src/domain/models/OrgSubscriptions";
import orgSubscriptionService from "@/src/domain/services/OrgSubscriptionService";
import { OrgSubscriptionType } from "@prisma/client";
import Stripe from "stripe";
import { inngest } from "./client";

export enum StripeEvent {
  STRIPE_WEBHOOK_RECEIVED = "stripe.webhook.received",
}

enum StripeWebhookEventType {
  CHECKOUT_SESSION_COMPLETED = "checkout.session.completed",
  CUSTOMER_SUBSCRIPTION_UPDATED = "customer.subscription.updated",
  CUSTOMER_SUBSCRIPTION_DELETED = "customer.subscription.deleted",
  INVOICE_PAYMENT_SUCCEEDED = "invoice.payment_succeeded",
}

export interface StripeWebhookReceivedPayload {
  stripeEvent: Stripe.Event;
}

export const stripeWebhookReceived = inngest.createFunction(
  { id: "stripe-webhook-received" },
  { event: StripeEvent.STRIPE_WEBHOOK_RECEIVED },
  async ({ event, step }) => {
    const eventPayload = event.data as StripeWebhookReceivedPayload;
    const { stripeEvent } = eventPayload;

    switch (stripeEvent.type) {
      case StripeWebhookEventType.CHECKOUT_SESSION_COMPLETED:
        await handleCheckoutSessionCompletedEvent(step, stripeEvent);
        break;
      case StripeWebhookEventType.CUSTOMER_SUBSCRIPTION_UPDATED:
        await handleCustomerSubscriptionUpdatedEvent(step, stripeEvent);
        break;
      case StripeWebhookEventType.CUSTOMER_SUBSCRIPTION_DELETED:
        await handleCustomerSubscriptionDeletedEvent(step, stripeEvent);
        break;
      case StripeWebhookEventType.INVOICE_PAYMENT_SUCCEEDED:
        await handleInvoicePaymentSucceeded(step, stripeEvent);
        break;
    }
  }
);

const handleCheckoutSessionCompletedEvent = async (
  step: any,
  event: Stripe.Event
) => {
  const session = event.data.object as Stripe.Checkout.Session;
  const orgId = session.client_reference_id;
  if (!orgId) {
    return;
  }

  const subscriptionId = session.subscription as string;
  const externalOrgSubscription: ExternalOrgSubscription = await step.run(
    "fetch-external-subscription",
    async () => {
      return await stripeAdapter.fetchExternalSubscription(subscriptionId);
    }
  );

  const {
    status,
    periodEndDate,
    externalSubscriptionId,
    externalCustomerId,
    dataUsageLimitInGb,
    metadata,
  } = externalOrgSubscription;
  await step.run("update-org-subscription", async () => {
    return await orgSubscriptionService.updateOrgSubscription({
      orgId,
      type: OrgSubscriptionType.PAID,
      status,
      periodEndDate,
      externalSubscriptionId,
      externalCustomerId,
      dataUsageLimitInGb,
      metadata,
    });
  });
};

const handleCustomerSubscriptionUpdatedEvent = async (
  step: any,
  event: Stripe.Event
) => {};

const handleCustomerSubscriptionDeletedEvent = async (
  step: any,
  event: Stripe.Event
) => {};

const handleInvoicePaymentSucceeded = async (
  step: any,
  event: Stripe.Event
) => {
  // const session = event.data.object as Stripe.Checkout.Session;
  // const subscription = await stripe.subscriptions.retrieve(
  //   session.subscription as string
  // );
};
