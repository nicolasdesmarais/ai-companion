import {
  ListAIsRequestParams,
  ListAIsRequestScope,
} from "@/src/domain/ports/api/ListAIsRequestParams";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextRequest, NextResponse } from "next/server";

async function getHandler(
  req: NextRequest,
  context: { orgId: string; userId: string }
) {
  const { orgId, userId } = context;

  const { searchParams } = new URL(req.url);

  const scopeParam = searchParams.get("scope");
  let scope: ListAIsRequestScope | undefined;

  if (
    !scopeParam ||
    !Object.values(ListAIsRequestScope).includes(
      scopeParam as ListAIsRequestScope
    )
  ) {
    scope = undefined;
  } else {
    scope = ListAIsRequestScope[scopeParam as keyof typeof ListAIsRequestScope];
  }

  const requestParams: ListAIsRequestParams = {
    scope,
    groupId: searchParams.get("groupId"),
    categoryId: searchParams.get("categoryId"),
    search: searchParams.get("search"),
  };

  const ais = await aiService.findAIsForUser(orgId, userId, requestParams);

  return NextResponse.json(ais);
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.AI,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);
