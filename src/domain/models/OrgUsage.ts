export interface OrgUsage {
  orgId: string;
  dataTokensUsed: number;
  dataUsageTokenLimit: number | null;
  apiTokensUsed: number | null;
  apiUsageTokenLimit: number | null;
}
