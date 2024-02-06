import { OrgSubscriptionEdition } from "@prisma/client";

export interface OrgSubscriptionDto {
  orgId: string;
  createdAt: Date;
  updatedAt: Date;
  edition: OrgSubscriptionEdition;
  dataUsageTokenLimit: number | null;
  apiUsageTokenLimit: number | null;
}

export interface UpdateOrgSubscriptionInput {
  orgId: string;
  edition: string;
  externalId?: string;
}
