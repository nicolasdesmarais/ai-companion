import { OrgSubscriptionType } from "@prisma/client";

export interface OrgSubscriptionDto {
  orgId: string;
  createdAt: Date;
  updatedAt: Date;
  dataUsageLimitInTokens: number | null;
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
}

export interface UpdateOrgSubscriptionInput {
  orgId: string;
  type: OrgSubscriptionType;
  externalId: string | null;
  dataUsageLimitInGb?: number;
  apiUsageTokenLimit?: number;
}

export interface ExternalOrgSubscription {
  externalId: string;
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
  metadata?: any;
}
