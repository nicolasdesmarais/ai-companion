import { ForbiddenError } from "@/src/domain/errors/Errors";
import prismadb from "@/src/lib/prismadb";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";
import { CategoryType } from "@prisma/client";

export interface AICategoryTypeInterface {
  aiId: string;
  categoryType: CategoryType;
}

export class AICategoryService {
  public async performUpdateOrCreateAICategoryType(
    authorizationContext: AuthorizationContext,
    aiId: any,
    aiCategoryTypes: Array<CategoryType>
  ) {
    const hasWritePermission: boolean = BaseEntitySecurityService.hasPermission(
      authorizationContext,
      SecuredResourceType.AI,
      SecuredAction.WRITE,
      SecuredResourceAccessLevel.INSTANCE
    );
    if (!hasWritePermission) {
      throw new ForbiddenError("Forbidden");
    }

    await prismadb.$transaction(async (tx) => {
      await tx.aICategoryType.deleteMany({
        where: { aiId },
      });
      let createData = aiCategoryTypes.map((categoryType) => {
        return {
          aiId,
          categoryType,
        };
      });
      await tx.aICategoryType.createMany({
        data: createData,
        skipDuplicates: true,
      });
    });
  }
}

export const aiCategoryService = new AICategoryService();
