import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
import { OrgSubscriptionType } from "@prisma/client";
import { UpdateOrgSubscriptionInput } from "../models/OrgSubscriptions";
import { OrgSubscriptionRepository } from "../ports/outgoing/OrgSubscriptionRepository";

const DEFAULT_DATA_USAGE_GB_LIMIT = 0.5;
const DEFAULT_API_USAGE_TOKEN_LIMIT = null;

export class OrgSubscriptionService {
  constructor(private orgSubscriptionRepository: OrgSubscriptionRepository) {}

  public async createInitialOrgSubscription(orgId: string) {
    return await this.orgSubscriptionRepository.createOrgSubscription(
      orgId,
      OrgSubscriptionType.FREE,
      DEFAULT_DATA_USAGE_GB_LIMIT,
      DEFAULT_API_USAGE_TOKEN_LIMIT
    );
  }

  public async updateOrgSubscription(input: UpdateOrgSubscriptionInput) {
    const { orgId, type, dataUsageLimitInGb, apiUsageTokenLimit, externalId } =
      input;
    await this.orgSubscriptionRepository.upsertOrgSubscription(
      orgId,
      type,
      dataUsageLimitInGb,
      apiUsageTokenLimit,
      externalId
    );
  }
}

const orgSubscriptionRepository = new OrgSubscriptionRepositoryImpl();
const orgSubscriptionService = new OrgSubscriptionService(
  orgSubscriptionRepository
);
export default orgSubscriptionService;
