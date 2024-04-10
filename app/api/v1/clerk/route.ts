import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { clerkClient } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";

export interface UserMetaDataInterface {
  value: string;
  key: string;
}

async function postHandler(
    request: Request,
    context: {
      authorizationContext: AuthorizationContext;
    }
) {
  const { value, key }: UserMetaDataInterface = await request.json();
  const { authorizationContext } = context;

  if (key === "superuser") {
    return NextResponse.json({
      success: false,
      message: "Cannot set superuser metadata",
    });
  }

  await clerkClient.users.updateUserMetadata(authorizationContext.userId, {
    publicMetadata: { [key]: value },
  });
  return NextResponse.json({ success: true });
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    postHandler
  )
);
