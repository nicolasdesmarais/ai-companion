import { OrgSubscriptionDto } from "../../models/OrgSubscriptions";

export interface OrgSubscriptionRepository {
  findOrCreateByOrgId(orgId: string): Promise<OrgSubscriptionDto | null>;
}
