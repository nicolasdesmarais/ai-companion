import groupService from "@/src/domain/services/GroupService";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { NextResponse } from "next/server";

export async function putHandler(
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

  const groups = await groupService.findGroupsByUser(authorizationContext);
  return NextResponse.json(groups);
}
