import {
  GroupDetailDto,
  UpdateGroupRequest,
} from "@/src/domain/ports/api/GroupsApi";
import groupService from "@/src/domain/services/GroupService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

async function getHandler(
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

  const group: GroupDetailDto | null = await groupService.findGroupById(
    authorizationContext,
    params.groupId
  );
  if (!group) {
    return new NextResponse("Group not found", { status: 404 });
  }

  return NextResponse.json(group);
}

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

  const updateGroupRequest: UpdateGroupRequest = await req.json();

  const updatedGroup: GroupDetailDto | null = await groupService.updateGroup(
    authorizationContext,
    params.groupId,
    updateGroupRequest
  );

  if (!updatedGroup) {
    throw new Error("Error updating group");
  }

  return NextResponse.json(updatedGroup);
}

async function deleteHandler(
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

  await groupService.deleteGroup(authorizationContext, params.groupId);
  const groups = await groupService.findGroupsByUser(authorizationContext);
  return NextResponse.json(groups);
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.GROUPS,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);

export const PUT = withErrorHandler(
  withAuthorization(
    SecuredResourceType.GROUPS,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    putHandler
  )
);

export const DELETE = withErrorHandler(
  withAuthorization(
    SecuredResourceType.GROUPS,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    deleteHandler
  )
);
