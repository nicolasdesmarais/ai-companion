import { OrgSubscriptionEdition } from "@prisma/client";

export interface OrgSubscriptionDto {
  orgId: String;
  createdAt: Date;
  updatedAt: Date;
  edition: OrgSubscriptionEdition;
  dataUsageTokenLimit: number | null;
  apiUsageTokenLimit: number | null;
}
