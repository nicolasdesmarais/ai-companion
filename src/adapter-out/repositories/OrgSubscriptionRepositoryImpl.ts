import { OrgSubscriptionDto } from "@/src/domain/models/OrgSubscriptions";
import { OrgSubscriptionRepository } from "@/src/domain/ports/outgoing/OrgSubscriptionRepository";
import prismadb from "@/src/lib/prismadb";
import { OrgSubscription, OrgSubscriptionEdition } from "@prisma/client";

const DEFAULT_EDITION = OrgSubscriptionEdition.FREE;
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
  public async findOrCreateByOrgId(
    orgId: string
  ): Promise<OrgSubscriptionDto | null> {
    let orgSubscription = await prismadb.orgSubscription.findUnique({
      where: { orgId },
    });
    if (!orgSubscription) {
      orgSubscription = await this.createDefaultOrgSubscription(orgId);
    }
    return mapOrgSubscriptionToDto(orgSubscription);
  }

  private async createDefaultOrgSubscription(
    orgId: string
  ): Promise<OrgSubscription> {
    return await prismadb.orgSubscription.create({
      data: {
        orgId,
        edition: DEFAULT_EDITION,
        dataUsageTokenLimit: DEFAULT_DATA_USAGE_TOKEN_LIMIT,
      },
    });
  }
}
