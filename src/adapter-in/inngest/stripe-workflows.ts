import orgSubscriptionService from "@/src/domain/services/OrgSubsriptionService";
import { stripe } from "@/src/lib/stripe";
import Stripe from "stripe";
import { inngest } from "./client";

export enum StripeEvent {
  STRIPE_WEBHOOK_RECEIVED = "stripe.webhook.received",
}

enum StripeWebhookEventType {
  CHECKOUT_SESSION_COMPLETED = "checkout.session.completed",
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

  const stripeSubscription: Stripe.Subscription = await step.run(
    "fetch-stripe-subscription",
    async () => {
      return await stripe.subscriptions.retrieve(
        session.subscription as string
      );
    }
  );

  const dataUsageLimitMetadata = stripeSubscription.metadata.allowance_gb;

  const dataUsageLimitInGb = dataUsageLimitMetadata
    ? parseInt(dataUsageLimitMetadata)
    : undefined;

  await step.run("update-org-subscription", async () => {
    return await orgSubscriptionService.updateOrgSubscription({
      orgId,
      externalId: stripeSubscription.id,
      dataUsageLimitInGb,
    });
  });
};

const handleInvoicePaymentSucceeded = async (
  step: any,
  event: Stripe.Event
) => {
  // const session = event.data.object as Stripe.Checkout.Session;
  // const subscription = await stripe.subscriptions.retrieve(
  //   session.subscription as string
  // );
};
