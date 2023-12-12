import groupService from "@/src/domain/services/GroupService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

async function putHandler(
  req: Request,
  context: {
    params: { groupId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;
  if (!params.groupId) {
    return new NextResponse("Group ID required", { status: 400 });
  }

  await groupService.leaveGroup(authorizationContext, params.groupId);

  return new NextResponse(null, { status: 204 });
}

export const PUT = withErrorHandler(
  withAuthorization(
    SecuredResourceType.GROUPS,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    putHandler
  )
);
