import { OrgSubscriptionStatus, OrgSubscriptionType } from "@prisma/client";

export interface OrgSubscriptionDto {
  orgId: string;
  createdAt: Date;
  updatedAt: Date;
  type: OrgSubscriptionType;
  status: OrgSubscriptionStatus;
  periodEndDate: Date | null;
  externalSubscriptionId: string | null;
  externalCustomerId: string | null;
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
  metadata?: any;
}

export interface UpdateOrgSubscriptionInput {
  type: OrgSubscriptionType;
  status: OrgSubscriptionStatus;
  periodEndDate: Date | null;
  externalSubscriptionId: string | null;
  externalCustomerId: string | null;
  dataUsageLimitInGb?: number | null;
  apiUsageTokenLimit?: number | null;
  metadata: any;
}

export interface ExternalOrgSubscription {
  status: OrgSubscriptionStatus;
  periodEndDate: Date | null;
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
