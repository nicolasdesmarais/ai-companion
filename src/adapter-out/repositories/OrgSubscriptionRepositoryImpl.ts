import { OrgSubscriptionDto } from "@/src/domain/models/OrgSubscriptions";
import { OrgSubscriptionRepository } from "@/src/domain/ports/outgoing/OrgSubscriptionRepository";
import prismadb from "@/src/lib/prismadb";
import { OrgSubscription } from "@prisma/client";

const DEFAULT_DATA_USAGE_TOKEN_LIMIT = 2500000;

const mapOrgSubscriptionToDto = (
  orgSubscription: OrgSubscription
): OrgSubscriptionDto => {
  const { id, ...orgSubscriptionWithoutId } = orgSubscription;
  return orgSubscriptionWithoutId;
};

export class OrgSubscriptionRepositoryImpl
  implements OrgSubscriptionRepository
{
  public async findByOrgId(orgId: string): Promise<OrgSubscriptionDto | null> {
    const orgSubscription = await prismadb.orgSubscription.findUnique({
      where: { orgId },
    });
    if (!orgSubscription) {
      return null;
    }
    return mapOrgSubscriptionToDto(orgSubscription);
  }

  public async findOrCreateByOrgId(
    orgId: string
  ): Promise<OrgSubscriptionDto | null> {
    const existingOrgSubscription = await this.findByOrgId(orgId);
    if (existingOrgSubscription) {
      return existingOrgSubscription;
    }

    const newOrgSubscription = await this.createDefaultOrgSubscription(orgId);
    return mapOrgSubscriptionToDto(newOrgSubscription);
  }

  private async createDefaultOrgSubscription(
    orgId: string
  ): Promise<OrgSubscription> {
    return await prismadb.orgSubscription.create({
      data: {
        orgId,
        dataUsageLimitInGb: DEFAULT_DATA_USAGE_TOKEN_LIMIT,
      },
    });
  }

  public async upsertOrgSubscription(
    orgId: string,
    dataUsageLimitInGb?: number,
    apiUsageTokenLimit?: number,
    externalId?: string
  ): Promise<OrgSubscriptionDto> {
    const updatedOrgSubscription = await prismadb.orgSubscription.upsert({
      where: { orgId },
      update: {
        externalId,
        dataUsageLimitInGb,
        apiUsageTokenLimit,
      },
      create: {
        orgId,
        externalId,
        dataUsageLimitInGb,
        apiUsageTokenLimit,
      },
    });

    return mapOrgSubscriptionToDto(updatedOrgSubscription);
  }
}
