import { OrgSubscriptionType } from "@prisma/client";
import { OrgSubscriptionDto } from "../../models/OrgSubscriptions";

export interface OrgSubscriptionRepository {
  findByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;

  findOrCreateByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;

  createOrgSubscription(
    orgId: string,
    type: OrgSubscriptionType,
    dataUsageLimitInGb: number,
    apiUsageTokenLimit: number | null
  ): Promise<OrgSubscriptionDto>;

  upsertOrgSubscription(
    orgId: string,
    type: OrgSubscriptionType,
    externalSubscriptionId?: string | null,
    externalCustomerId?: string | null,
    dataUsageLimitInGb?: number | null,
    apiUsageTokenLimit?: number | null,
    metadata?: any
  ): Promise<OrgSubscriptionDto>;
}
