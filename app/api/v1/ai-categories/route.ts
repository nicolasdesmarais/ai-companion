import {NextRequest, NextResponse} from "next/server";
import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";
import {aiCategoryService, AICategoryTypeInterface} from "@/src/domain/services/AICategoryService";
import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import {withAuthorization} from "@/src/middleware/AuthorizationMiddleware";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";

async function postHandler(
    request: NextRequest,
    context: {
        params: { aiCategoryTypes: Array<AICategoryTypeInterface> };
        authorizationContext: AuthorizationContext;
    }
) {
   const { params, authorizationContext } = context;
   const aiCategoryTypes : Array<AICategoryTypeInterface> = params.aiCategoryTypes;
   const response = await aiCategoryService.performUpdateOrCreateAICategoryType(authorizationContext, aiCategoryTypes);
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