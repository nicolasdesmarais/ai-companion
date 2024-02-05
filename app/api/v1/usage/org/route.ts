import usageService from "@/src/domain/services/UsageService";
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
  const orgUsage = await usageService.getOrgUsage(authorizationContext);
  return NextResponse.json(orgUsage);
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.ORG_USAGE,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);
