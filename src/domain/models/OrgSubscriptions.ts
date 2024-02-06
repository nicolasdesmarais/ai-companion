export interface OrgSubscriptionDto {
  orgId: string;
  createdAt: Date;
  updatedAt: Date;
  dataUsageLimitInGb: number | null;
  apiUsageTokenLimit: number | null;
}

export interface UpdateOrgSubscriptionInput {
  orgId: string;
  externalId?: string;
  dataUsageLimitInGb?: number;
  apiUsageTokenLimit?: number;
}
