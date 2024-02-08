import { DataSourceRepositoryImpl } from "@/src/adapter-out/repositories/DataSourceRepositoryImpl";
import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
import {
  convertGigabytesToTokens,
  convertTokensToGigabytes,
} from "@/src/lib/tokenCount";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { UsageSecurityService } from "@/src/security/services/UsageSecurityService";
import { ForbiddenError } from "../errors/Errors";
import { OrgUsage, OrgUsageByAI } from "../models/OrgUsage";
import { DataSourceRepository } from "../ports/outgoing/DataSourceRepository";
import { OrgSubscriptionRepository } from "../ports/outgoing/OrgSubscriptionRepository";

export class UsageService {
  constructor(
    private dataSourceRepository: DataSourceRepository,
    private orgSubscriptionRepository: OrgSubscriptionRepository
  ) {}

  public async getOrgUsage(
    authorizationContext: AuthorizationContext
  ): Promise<OrgUsage> {
    const { orgId } = authorizationContext;
    const canViewOrgUsage = UsageSecurityService.canViewOrgUsage(
      authorizationContext,
      orgId
    );
    if (!canViewOrgUsage) {
      throw new ForbiddenError();
    }

    const orgSubscription = await this.orgSubscriptionRepository.findByOrgId(
      orgId
    );

    const dataUsageLimitInGb = orgSubscription
      ? orgSubscription.dataUsageLimitInGb
      : null;
    const apiUsageTokenLimit = orgSubscription
      ? orgSubscription.apiUsageTokenLimit
      : null;

    const dataTokensUsed =
      await this.dataSourceRepository.getNumberOfTokensStoredForOrg(orgId);
    const dataUsedInGb = convertTokensToGigabytes(dataTokensUsed);

    const apiTokensUsed = 0; // TODO: Implement API usage tracking

    return {
      orgId,
      dataUsedInGb,
      dataUsageLimitInGb,
      apiTokensUsed,
      apiUsageTokenLimit,
    };
  }

  public async getAIUsage(
    authorizationContext: AuthorizationContext
  ): Promise<OrgUsageByAI> {
    const orgUsage = await this.getOrgUsage(authorizationContext);
    const aiDataUsages =
      await this.dataSourceRepository.getNumberOfTokensStoredForOrgPerAi(
        authorizationContext.orgId
      );
    const aiUsages = aiDataUsages.map((aiDataUsage) => {
      return {
        aiId: aiDataUsage.aiId,
        aiDataTokensUsed: aiDataUsage.aiDataTokensUsed,
        aiApiTokensUsed: 0, // TODO: Implement API usage tracking
      };
    });

    return {
      orgUsage,
      aiUsages,
    };
  }

  public async hasSufficientDataStorage(
    orgId: string,
    tokensToAdd: number
  ): Promise<boolean> {
    const orgSubscription =
      await this.orgSubscriptionRepository.findOrCreateByOrgId(orgId);

    const dataUsageTokenLimit = orgSubscription?.dataUsageLimitInGb
      ? convertGigabytesToTokens(orgSubscription.dataUsageLimitInGb)
      : undefined;

    const orgTokenCount =
      await this.dataSourceRepository.getNumberOfTokensStoredForOrg(orgId);

    if (!dataUsageTokenLimit) {
      // No limit set, meaning unlimited usage is allowed
      return true;
    }

    return Number(orgTokenCount) + Number(tokensToAdd) <= dataUsageTokenLimit;
  }
}

const dataSourceRepository = new DataSourceRepositoryImpl();
const orgSubscriptionRepository = new OrgSubscriptionRepositoryImpl();
const usageService = new UsageService(
  dataSourceRepository,
  orgSubscriptionRepository
);
export default usageService;
