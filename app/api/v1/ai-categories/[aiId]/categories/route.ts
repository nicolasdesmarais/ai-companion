import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import {NextRequest, NextResponse} from "next/server";
import {withAuthorization} from "@/src/middleware/AuthorizationMiddleware";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";
import {aiCategoryService, AICategoryTypeInterface} from "@/src/domain/services/AICategoryService";

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

export const GET = withErrorHandler(
    withAuthorization(
        SecuredResourceType.AI,
        SecuredAction.READ,
        [SecuredResourceAccessLevel.INSTANCE],
        getHandler
    )
);