export interface OrgUsage {
  orgId: string;
  dataTokensUsed: number;
  dataUsageTokenLimit: number | null;
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
