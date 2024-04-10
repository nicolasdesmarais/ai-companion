import {withAuthorization} from "@/src/middleware/AuthorizationMiddleware";
import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {clerkClient} from "@clerk/nextjs";
import {NextRequest, NextResponse} from "next/server";
import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";
import {UserMetaDataInterface} from "@/src/domain/services/ClerkService";
import {clerkService} from "@/src/domain/services/ClerkService";
import {User} from "@clerk/nextjs/server";

async function postHandler(
    request: Request,
    context: {
      params: { userId: string };
      authorizationContext: AuthorizationContext;
    }
) {
  const sortKeyValue : UserMetaDataInterface = await request.json();
  const { params, authorizationContext } = context;
  const userId = params.userId;
  if (sortKeyValue.key === "superuser") {
    return NextResponse.json({
      success: false,
      message: "Cannot set superuser metadata",
    });
  }
  const user: User = await clerkService.updateUserMetadata(authorizationContext, userId, sortKeyValue);
  return NextResponse.json(user);
}

async function getHandler(
    request: NextRequest,
    context: {
      params: { userId: string };
      authorizationContext: AuthorizationContext;
    }
) {
  const { params, authorizationContext} = context;
  const userId = params.userId;
  const user: User = await clerkService.getClerkUser(authorizationContext, userId);
    return NextResponse.json(user);
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
