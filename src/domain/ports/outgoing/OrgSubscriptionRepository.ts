import { OrgSubscriptionEdition } from "@prisma/client";
import { OrgSubscriptionDto } from "../../models/OrgSubscriptions";

export interface OrgSubscriptionRepository {
  findByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;
  findOrCreateByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;
  upsertOrgSubscription(
    orgId: string,
    edition?: OrgSubscriptionEdition,
    apiUsageTokenLimit?: number,
    dataUsageTokenLimit?: number,
    externalId?: string
  ): Promise<OrgSubscriptionDto>;
}
