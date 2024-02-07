import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { OrgSubscriptionType } from "@prisma/client";
import { EntityNotFoundError } from "../errors/Errors";
import {
  OrgSubscriptionDto,
  UpdateOrgSubscriptionInput,
} from "../models/OrgSubscriptions";
import { OrgSubscriptionRepository } from "../ports/outgoing/OrgSubscriptionRepository";

const DEFAULT_DATA_USAGE_GB_LIMIT = 0.5;
const DEFAULT_API_USAGE_TOKEN_LIMIT = null;

export class OrgSubscriptionService {
  constructor(private orgSubscriptionRepository: OrgSubscriptionRepository) {}

  public async getOrgSubscription(
    authorizationContext: AuthorizationContext
  ): Promise<OrgSubscriptionDto | null> {
    const { orgId } = authorizationContext;
    const orgSubscription = await this.orgSubscriptionRepository.findByOrgId(
      orgId
    );
    if (!orgSubscription) {
      throw new EntityNotFoundError(
        `OrgSubscription not found for orgId: ${orgId}`
      );
    }

    return orgSubscription;
  }

  public async createInitialOrgSubscription(orgId: string) {
    return await this.orgSubscriptionRepository.createOrgSubscription(
      orgId,
      OrgSubscriptionType.FREE,
      DEFAULT_DATA_USAGE_GB_LIMIT,
      DEFAULT_API_USAGE_TOKEN_LIMIT
    );
  }

  public async updateOrgSubscription(
    input: UpdateOrgSubscriptionInput
  ): Promise<OrgSubscriptionDto> {
    const {
      orgId,
      type,
      externalId,
      dataUsageLimitInGb,
      apiUsageTokenLimit,
      metadata,
    } = input;

    return await this.orgSubscriptionRepository.upsertOrgSubscription(
      orgId,
      type,
      dataUsageLimitInGb,
      apiUsageTokenLimit,
      externalId,
      metadata
    );
  }
}

const orgSubscriptionRepository = new OrgSubscriptionRepositoryImpl();
const orgSubscriptionService = new OrgSubscriptionService(
  orgSubscriptionRepository
);
export default orgSubscriptionService;
