import { DataSourceRepositoryImpl } from "@/src/adapter-out/repositories/DataSourceRepositoryImpl";
import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { OrgUsage } from "../models/OrgUsage";
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
    const orgId = authorizationContext.orgId;

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
