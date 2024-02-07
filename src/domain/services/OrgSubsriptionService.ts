import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
import { UpdateOrgSubscriptionInput } from "../models/OrgSubscriptions";
import { OrgSubscriptionRepository } from "../ports/outgoing/OrgSubscriptionRepository";

export class OrgSubscriptionService {
  constructor(private orgSubscriptionRepository: OrgSubscriptionRepository) {}

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
