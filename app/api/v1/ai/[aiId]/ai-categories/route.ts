import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import {NextRequest, NextResponse} from "next/server";
import {withAuthorization} from "@/src/middleware/AuthorizationMiddleware";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";
import {aiCategoryService, AICategoryTypeInterface} from "@/src/domain/services/AICategoryService";
import {CategoryType} from "@prisma/client";

async function getHandler(
    request: NextRequest,
    context: {
        params: { aiId: string };
        authorizationContext: AuthorizationContext;
    }
) {
    const { params, authorizationContext } = context;
    const  aiId = params.aiId
    const aiCategories : Array<AICategoryTypeInterface> = await aiCategoryService.getAICategoryTypes(authorizationContext, aiId);
    return NextResponse.json(aiCategories);
}

async function postHandler(
    request: Request,
    context: {
        params: { aiId : string, categoryTypes : Array<CategoryType> };
        authorizationContext: AuthorizationContext;
    }
) {
    const categoryTypes : Array<CategoryType> = await request.json();
    const { params, authorizationContext } = context;
    const aiId = params.aiId;
    const response = await aiCategoryService.performUpdateOrCreateAICategoryType(authorizationContext, aiId, categoryTypes);
    return NextResponse.json({status: 'success', data: response});
}

export const POST = withErrorHandler(
    withAuthorization(
        SecuredResourceType.AI,
        SecuredAction.WRITE,
        [SecuredResourceAccessLevel.INSTANCE],
        postHandler
    )
);

export const GET = withErrorHandler(
    withAuthorization(
        SecuredResourceType.AI,
        SecuredAction.READ,
        [SecuredResourceAccessLevel.INSTANCE],
        getHandler
    )
);