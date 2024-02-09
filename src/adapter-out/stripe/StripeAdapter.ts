import { ExternalOrgSubscription } from "@/src/domain/models/OrgSubscriptions";
import { stripe } from "@/src/lib/stripe";
import { OrgSubscriptionStatus } from "@prisma/client";

export interface StripeMetadata {
  productId: string;
  productName: string;
  productMetadata: any;
}

export interface OrgSubscriptionMetadata {
  orgId: string;
}

export class StripeAdapter {
  public async fetchExternalSubscription(
    subscriptionId: string
  ): Promise<ExternalOrgSubscription> {
    const subscription = await stripe.subscriptions.retrieve(subscriptionId);

    if (subscription.items.data.length === 0) {
      throw new Error("Subscription has no items");
    }

    const status: OrgSubscriptionStatus =
      subscription.status === "canceled"
        ? OrgSubscriptionStatus.CANCELLED
        : OrgSubscriptionStatus.ACTIVE;
    const periodEndDate: Date = new Date(
      subscription.current_period_end * 1000
    );

    const externalCustomerId = subscription.customer as string;
    const productId = subscription.items.data[0].plan.product as string;
    const product = await stripe.products.retrieve(productId);

    const dataUsageLimitMetadata = product.metadata.allowance_gb;
    const dataUsageLimitInGb = dataUsageLimitMetadata
      ? parseInt(dataUsageLimitMetadata)
      : null;
    const apiUsageTokenLimit = null; // TODO: fetch from stripe

    const metadata: StripeMetadata = {
      productId: product.id,
      productName: product.name,
      productMetadata: product.metadata,
    };

    return {
      status,
      periodEndDate,
      externalSubscriptionId: subscriptionId,
      externalCustomerId,
      dataUsageLimitInGb,
      apiUsageTokenLimit,
      metadata,
    };
  }

  public async createManageSubscriptionSession(
    customerId: string,
    redirectUrl: string
  ): Promise<string> {
    const stripeSession = await stripe.billingPortal.sessions.create({
      customer: customerId,
      return_url: redirectUrl,
    });

    return stripeSession.url;
  }
}

const stripeAdapter = new StripeAdapter();
export default stripeAdapter;
