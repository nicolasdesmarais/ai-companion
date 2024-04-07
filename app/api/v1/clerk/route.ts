import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { clerkClient } from "@clerk/nextjs";
import {NextRequest, NextResponse} from "next/server";
import {User} from "@clerk/nextjs/server";

export interface UserMetaDataInterface {
  value: string;
  key: string;
}

async function postHandler(request: Request) {
  const authorizationContext = getUserAuthorizationContext();
  if (!authorizationContext) {
    return;
  }
  const { value, key }: UserMetaDataInterface = await request.json();

  await clerkClient.users.updateUserMetadata(authorizationContext.userId, {
    publicMetadata: { [key]: value },
  });

  const data: User = await clerkClient.users.getUser(authorizationContext.userId);
  return NextResponse.json({data : data, success: true});
}

async function getHandler(request: NextRequest) {
  const authorizationContext = getUserAuthorizationContext();
  if (!authorizationContext) {
    return;
  }
  const { searchParams } = new URL(request.url);
  const scopeParam : string = searchParams.get("userId") || authorizationContext.userId;
  const data: User = await clerkClient.users.getUser(scopeParam);
  return NextResponse.json(data);
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    postHandler
  )
);

export const GET = withErrorHandler(
    withAuthorization(
        SecuredResourceType.AI,
        SecuredAction.READ,
        Object.values(SecuredResourceAccessLevel),
        getHandler
    )
);
