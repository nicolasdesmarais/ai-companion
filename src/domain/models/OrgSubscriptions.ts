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
  externalId?: string;
  dataUsageLimitInGb?: number;
  apiUsageTokenLimit?: number;
}

export interface OrgSubscriptionUsageLimits {
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
}
