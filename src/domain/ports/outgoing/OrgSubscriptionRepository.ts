import { OrgSubscriptionDto } from "../../models/OrgSubscriptions";

export interface OrgSubscriptionRepository {
  findByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;
  findOrCreateByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;
  upsertOrgSubscription(
    orgId: string,
    dataUsageLimitInGb?: number,
    apiUsageTokenLimit?: number,
    externalId?: string
  ): Promise<OrgSubscriptionDto>;
}
