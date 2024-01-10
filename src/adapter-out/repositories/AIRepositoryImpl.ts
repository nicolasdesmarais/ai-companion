import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { AIRepository } from "@/src/domain/ports/outgoing/AIRepository";
import prismadb from "@/src/lib/prismadb";
import { AI } from "@prisma/client";

export class AIRepositoryImpl implements AIRepository {
  public async getById(id: string): Promise<AI> {
    const ai = await prismadb.aI.findUnique({
      where: { id },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${id} not found`);
    }

    return ai;
  }

  public async approveAIForOrg(aiId: string, orgId: string): Promise<void> {
    await prismadb.organizationApprovedAI.upsert({
      where: {
        orgId_aiId: {
          orgId,
          aiId,
        },
      },
      create: {
        aiId,
        orgId,
      },
      update: {},
    });
  }

  public async revokeAIForOrg(aiId: string, orgId: string): Promise<void> {
    await prismadb.organizationApprovedAI.delete({
      where: {
        orgId_aiId: {
          orgId,
          aiId,
        },
      },
    });
  }
}
