export interface OrgUsage {
  orgId: string;
  dataUsedInGb: number;
  dataUsageLimitInGb: number | null;
  apiTokensUsed: number | null;
  apiUsageTokenLimit: number | null;
}

export interface OrgUsageByAI {
  orgUsage: OrgUsage;
  aiUsages: AIUsage[];
}

export interface AIUsage {
  aiId: string;
  aiDataTokensUsed: number | null;
  aiApiTokensUsed: number | null;
}

export interface AIDataUsage {
  aiId: string;
  aiDataTokensUsed: number | null;
}
