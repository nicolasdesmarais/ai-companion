import { DataSourceRepositoryImpl } from "@/src/adapter-out/repositories/DataSourceRepositoryImpl";
import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
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

    const dataTokensUsed =
      await this.dataSourceRepository.getNumberOfTokensStoredForOrg(orgId);
    const orgSubscription = await this.orgSubscriptionRepository.findByOrgId(
      orgId
    );
    const dataUsageTokenLimit = orgSubscription
      ? orgSubscription.dataUsageTokenLimit
      : null;
    const apiUsageTokenLimit = orgSubscription
      ? orgSubscription.apiUsageTokenLimit
      : null;

    const apiTokensUsed = 0; // TODO: Implement API usage tracking

    return {
      orgId,
      dataTokensUsed,
      dataUsageTokenLimit,
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
    const dataUsageTokenLimit = orgSubscription?.dataUsageTokenLimit;
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
