import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import {withAuthorization} from "@/src/middleware/AuthorizationMiddleware";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {NextResponse} from "next/server";
import clerkService from "@/src/domain/services/ClerkService";
import {ClerkUserIdInterface} from "@/components/search-input";

async function postHandler(request: Request)  {
    const { clerkUserId } : ClerkUserIdInterface = await request.json();
    const user= await clerkService.getUserMetadata(clerkUserId);
    return NextResponse.json({ user });
}

export const POST = withErrorHandler(
    withAuthorization(
        SecuredResourceType.ORG_USAGE,
        SecuredAction.WRITE,
        Object.values(SecuredResourceAccessLevel),
        postHandler));






