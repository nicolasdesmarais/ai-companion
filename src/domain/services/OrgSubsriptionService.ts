import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
import { UpdateOrgSubscriptionInput } from "../models/OrgSubscriptions";
import { OrgSubscriptionRepository } from "../ports/outgoing/OrgSubscriptionRepository";

export class OrgSubscriptionService {
  constructor(private orgSubscriptionRepository: OrgSubscriptionRepository) {}

  public async updateOrgSubscription(input: UpdateOrgSubscriptionInput) {
    // const edition = OrgSubscriptionEdition.FREE; // TODO - get edition from external service
    // const dataUsageTokenLimit = 2500000; // TODO - get dataUsageTokenLimit from external service
    // const apiUsageTokenLimit = 2500000; // TODO - get apiUsageTokenLimit from external service

    await this.orgSubscriptionRepository.upsertOrgSubscription(
      input.orgId,
      undefined,
      undefined,
      undefined,
      input.externalId
    );
  }
}

const orgSubscriptionRepository = new OrgSubscriptionRepositoryImpl();
const orgSubscriptionService = new OrgSubscriptionService(
  orgSubscriptionRepository
);
export default orgSubscriptionService;
