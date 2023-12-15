import { ListGroupsResponse } from "@/src/adapter-in/api/GroupsApi";
import { GroupSummaryDto } from "@/src/domain/models/Groups";
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
  context: { authorizationContext: AuthorizationContext }
): Promise<NextResponse<ListGroupsResponse>> {
  const { authorizationContext } = context;

  const groups: GroupSummaryDto[] = await groupService.findGroupsByUser(
    authorizationContext
  );
  return NextResponse.json({ data: groups });
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.GROUPS,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);
