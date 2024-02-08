import { OrgSubscriptionRepositoryImpl } from "@/src/adapter-out/repositories/OrgSubscriptionRepositoryImpl";
import stripeAdapter from "@/src/adapter-out/stripe/StripeAdapter";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { OrgSubscriptionType } from "@prisma/client";
import {
  CreateManageSubscriptionSessionRequest,
  ManageSubscriptionSession,
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
  ): Promise<OrgSubscriptionDto> {
    const { orgId } = authorizationContext;
    let orgSubscription = await this.orgSubscriptionRepository.findByOrgId(
      orgId
    );
    if (!orgSubscription) {
      orgSubscription = await this.createInitialOrgSubscription(orgId);
    }

    if (orgSubscription.externalSubscriptionId) {
      const externalSubscription =
        await stripeAdapter.fetchExternalSubscription(
          orgSubscription.externalSubscriptionId
        );
      return {
        ...orgSubscription,
        dataUsageLimitInGb: externalSubscription.dataUsageLimitInGb,
        apiUsageTokenLimit: externalSubscription.apiUsageTokenLimit,
        metadata: externalSubscription.metadata,
      };
    }

    return orgSubscription;
  }

  public async createManageSubscriptionSession(
    authorizationContext: AuthorizationContext,
    input: CreateManageSubscriptionSessionRequest
  ): Promise<ManageSubscriptionSession> {
    const orgSubscription = await this.getOrgSubscription(authorizationContext);
    if (!orgSubscription.externalCustomerId) {
      throw new Error(
        `ExternalId not found for org subscription for orgId: ${orgSubscription.orgId}`
      );
    }

    const manageSubscriptionRedirectUrl =
      await stripeAdapter.createManageSubscriptionSession(
        orgSubscription.externalCustomerId,
        input.redirectUrl
      );

    return {
      manageSubscriptionRedirectUrl,
    };
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
      externalSubscriptionId,
      externalCustomerId,
      dataUsageLimitInGb,
      apiUsageTokenLimit,
      metadata,
    } = input;

    return await this.orgSubscriptionRepository.upsertOrgSubscription(
      orgId,
      type,
      externalSubscriptionId,
      externalCustomerId,
      dataUsageLimitInGb,
      apiUsageTokenLimit,
      metadata
    );
  }
}

const orgSubscriptionRepository = new OrgSubscriptionRepositoryImpl();
const orgSubscriptionService = new OrgSubscriptionService(
  orgSubscriptionRepository
);
export default orgSubscriptionService;
