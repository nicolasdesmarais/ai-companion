import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";
import {AISecurityService} from "@/src/security/services/AISecurityService";
import prismadb from "@/src/lib/prismadb";
import {ForbiddenError} from "@/src/domain/errors/Errors";
import {CategoryType} from "@prisma/client";
import {BaseEntitySecurityService} from "@/src/security/services/BaseEntitySecurityService";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";

export interface AICategoryTypeInterface {
    aiId: string;
    categoryType: CategoryType;
}

export class AICategoryService {
    public async getAICategoryTypes(
        authorizationContext: AuthorizationContext,
        aiId: string): Promise<Array<AICategoryTypeInterface>> {
        const hasReadPermission = AISecurityService.hasInstanceReadAccess(authorizationContext);
        if (!hasReadPermission) {
            throw new ForbiddenError("Forbidden");
        }
        const aiCategories : Array<AICategoryTypeInterface> = await prismadb.aICategoryType.findMany({where: {aiId: aiId}});
        return aiCategories;
    }

    public async getAIsByCategoryType(
        authorizationContext: AuthorizationContext,
        categoryType: CategoryType) : Promise<Array<AICategoryTypeInterface>> {
        const hasReadPermission = AISecurityService.hasInstanceReadAccess(authorizationContext);
        if (!hasReadPermission) {
            throw new ForbiddenError("Forbidden");
        }
        const aiCategories : Array<AICategoryTypeInterface> = await prismadb.aICategoryType.findMany({where: {categoryType: categoryType}});
        return aiCategories;
    }

    public async performUpdateOrCreateAICategoryType(
        authorizationContext: AuthorizationContext,
        aiCategoryTypes : Array<AICategoryTypeInterface>) {
        const hasWritePermission : boolean = BaseEntitySecurityService.hasPermission(
            authorizationContext,
            SecuredResourceType.AI,
            SecuredAction.WRITE,
            SecuredResourceAccessLevel.INSTANCE
        );
        if (!hasWritePermission) {
            throw new ForbiddenError("Forbidden");
        }
        let areRecordsDeleted : boolean = false;
        aiCategoryTypes.map(async (aiCategoryType) => {
            const aiId : any = aiCategoryType.aiId;
            const categoryType : CategoryType = aiCategoryType.categoryType;
            if (!areRecordsDeleted) {
                await prismadb.aICategoryType.deleteMany({where: {aiId: aiId}});
                areRecordsDeleted = true;
            }
            await prismadb.aICategoryType.create({data : { aiId, categoryType }});
        });
    }
}

export const aiCategoryService = new AICategoryService();