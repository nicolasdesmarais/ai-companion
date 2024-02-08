import { OrgSubscriptionType } from "@prisma/client";

export interface OrgSubscriptionDto {
  orgId: string;
  createdAt: Date;
  updatedAt: Date;
  externalId: string | null;
  dataUsageLimitInTokens: number | null;
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
  metadata?: any;
}

export interface UpdateOrgSubscriptionInput {
  orgId: string;
  type: OrgSubscriptionType;
  externalId: string | null;
  dataUsageLimitInGb?: number | null;
  apiUsageTokenLimit?: number | null;
  metadata: any;
}

export interface ExternalOrgSubscription {
  externalId: string;
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
  metadata?: any;
}

export interface CreateManageSubscriptionSessionRequest {
  redirectUrl: string;
}

export interface ManageSubscriptionSession {
  orgId: string;
  externalSubscriptionId: string;
  manageSubscriptionRedirectUrl: string;
}
