import { OrgSubscriptionDto } from "@/src/domain/models/OrgSubscriptions";
import { OrgSubscriptionRepository } from "@/src/domain/ports/outgoing/OrgSubscriptionRepository";
import prismadb from "@/src/lib/prismadb";
import { OrgSubscription, OrgSubscriptionType } from "@prisma/client";

const DEFAULT_DATA_USAGE_GB_LIMIT = 0.5;

const mapOrgSubscriptionToDto = (
  orgSubscription: OrgSubscription
): OrgSubscriptionDto => {
  const { id, ...orgSubscriptionWithoutId } = orgSubscription;
  return { ...orgSubscriptionWithoutId };
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

  public async createOrgSubscription(
    orgId: string,
    type: OrgSubscriptionType,
    dataUsageLimitInGb: number,
    apiUsageTokenLimit: number | null
  ): Promise<OrgSubscriptionDto> {
    const orgSubscription = await prismadb.orgSubscription.create({
      data: {
        orgId,
        type,
        dataUsageLimitInGb,
        apiUsageTokenLimit,
      },
    });

    return mapOrgSubscriptionToDto(orgSubscription);
  }

  private async createDefaultOrgSubscription(
    orgId: string
  ): Promise<OrgSubscription> {
    return await prismadb.orgSubscription.create({
      data: {
        orgId,
        type: OrgSubscriptionType.FREE,
        dataUsageLimitInGb: DEFAULT_DATA_USAGE_GB_LIMIT,
      },
    });
  }

  public async upsertOrgSubscription(
    orgId: string,
    type: OrgSubscriptionType,
    externalSubscriptionId?: string | null,
    externalCustomerId?: string | null,
    dataUsageLimitInGb?: number | null,
    apiUsageTokenLimit?: number | null,
    metadata?: any
  ): Promise<OrgSubscriptionDto> {
    const orgSubscriptionData = {
      type,
      externalSubscriptionId,
      externalCustomerId,
      dataUsageLimitInGb,
      apiUsageTokenLimit,
      metadata,
    };

    const updatedOrgSubscription = await prismadb.orgSubscription.upsert({
      where: { orgId },
      update: orgSubscriptionData,
      create: {
        orgId,
        ...orgSubscriptionData,
      },
    });

    return mapOrgSubscriptionToDto(updatedOrgSubscription);
  }
}
