import { OrgSubscriptionDto } from "../../models/OrgSubscriptions";

export interface OrgSubscriptionRepository {
  findByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;
  findOrCreateByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;
}
