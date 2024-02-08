import { OrgSubscriptionType } from "@prisma/client";

export interface OrgSubscriptionDto {
  orgId: string;
  createdAt: Date;
  updatedAt: Date;
  externalSubscriptionId: string | null;
  externalCustomerId: string | null;
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
  metadata?: any;
}

export interface UpdateOrgSubscriptionInput {
  orgId: string;
  type: OrgSubscriptionType;
  externalSubscriptionId: string | null;
  externalCustomerId: string | null;
  dataUsageLimitInGb?: number | null;
  apiUsageTokenLimit?: number | null;
  metadata: any;
}

export interface ExternalOrgSubscription {
  externalSubscriptionId: string;
  externalCustomerId: string;
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
  metadata?: any;
}

export interface CreateManageSubscriptionSessionRequest {
  redirectUrl: string;
}

export interface ManageSubscriptionSession {
  manageSubscriptionRedirectUrl: string;
}
