import { headers } from "next/headers";
import { NextResponse } from "next/server";
import Stripe from "stripe";

import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import {
  StripeEvent,
  StripeWebhookReceivedPayload,
} from "@/src/adapter-in/inngest/stripe-workflows";
import { stripe } from "@/src/lib/stripe";

enum StripeWebhookEventType {
  CHECKOUT_SESSION_COMPLETED = "checkout.session.completed",
  INVOICE_PAYMENT_SUCCEEDED = "invoice.payment_succeeded",
}

export async function POST(req: Request) {
  const body = await req.text();
  const signature = headers().get("Stripe-Signature") as string;

  let event: Stripe.Event;
  try {
    event = stripe.webhooks.constructEvent(
      body,
      signature,
      process.env.STRIPE_WEBHOOK_SECRET!
    );
  } catch (error: any) {
    return new NextResponse(`Webhook Error: ${error.message}`, { status: 400 });
  }

  const stripeWebhookReceivedEventPayload: StripeWebhookReceivedPayload = {
    stripeEvent: event,
  };
  await publishEvent(
    StripeEvent.STRIPE_WEBHOOK_RECEIVED,
    stripeWebhookReceivedEventPayload
  );

  return new NextResponse(null, { status: 200 });
}
