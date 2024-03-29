import {
  ListAIsRequestParams,
  ListAIsRequestScope,
} from "@/src/adapter-in/api/AIApi";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextRequest, NextResponse } from "next/server";

async function getHandler(
  req: NextRequest,
  context: { authorizationContext: AuthorizationContext }
) {
  const { authorizationContext } = context;

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

  let approvedByOrg;
  if (searchParams.has("approvedByOrg")) {
    approvedByOrg = searchParams.get("approvedByOrg") === "true";
  }

  const requestParams: ListAIsRequestParams = {
    scope,
    groupId: searchParams.get("groupId"),
    categoryId: searchParams.get("categoryId"),
    search: searchParams.get("search"),
    approvedByOrg,
  };

  const ais = await aiService.findAIsForUser(
    authorizationContext,
    requestParams
  );

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
