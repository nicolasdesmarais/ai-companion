import { DataSourceRepositoryImpl } from "@/src/adapter-out/repositories/DataSourceRepositoryImpl";
import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
import { DataSourceRepository } from "../ports/outgoing/DataSourceRepository";
import { OrgSubscriptionRepository } from "../ports/outgoing/OrgSubscriptionRepository";

export class UsageService {
  constructor(
    private dataSourceRepository: DataSourceRepository,
    private orgSubscriptionRepository: OrgSubscriptionRepository
  ) {}

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

    return orgTokenCount + tokensToAdd <= dataUsageTokenLimit;
  }
}

const dataSourceRepository = new DataSourceRepositoryImpl();
const orgSubscriptionRepository = new OrgSubscriptionRepositoryImpl();
const usageService = new UsageService(
  dataSourceRepository,
  orgSubscriptionRepository
);
export default usageService;
