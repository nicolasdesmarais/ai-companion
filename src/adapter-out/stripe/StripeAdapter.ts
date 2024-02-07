import { OrgSubscriptionUsageLimits } from "@/src/domain/models/OrgSubscriptions";
import { stripe } from "@/src/lib/stripe";

export class StripeAdapter {
  public async fetchUsageLimitsFromSubscription(
    subscriptionId: string
  ): Promise<OrgSubscriptionUsageLimits> {
    const subscription = await stripe.subscriptions.retrieve(subscriptionId);

    if (subscription.items.data.length === 0) {
      throw new Error("Subscription has no items");
    }

    const item = subscription.items.data[0];

    const dataUsageLimitMetadata = item.metadata.allowance_gb;
    const dataUsageLimitInGb = dataUsageLimitMetadata
      ? parseInt(dataUsageLimitMetadata)
      : null;
    const apiUsageTokenLimit = null; // TODO: fetch from stripe

    return {
      dataUsageLimitInGb,
      apiUsageTokenLimit,
    };
  }
}

const stripeAdapter = new StripeAdapter();
export default stripeAdapter;
