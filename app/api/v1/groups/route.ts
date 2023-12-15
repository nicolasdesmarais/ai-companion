import {
  CreateGroupRequest,
  GroupDetailDto,
} from "@/src/adapter-in/api/GroupsApi";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";
import groupService from "../../../../src/domain/services/GroupService";

async function postHandler(
  req: Request,
  context: { authorizationContext: AuthorizationContext }
) {
  const { authorizationContext } = context;

  const createGroupRequest: CreateGroupRequest = await req.json();

  const group: GroupDetailDto | null = await groupService.createGroup(
    authorizationContext,
    createGroupRequest
  );
  if (!group) {
    throw new Error("Error creating group");
  }

  return new NextResponse(JSON.stringify(group), { status: 201 });
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.GROUPS,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    postHandler
  )
);
