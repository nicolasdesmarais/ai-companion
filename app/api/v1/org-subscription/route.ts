import { CreateManageSubscriptionSessionRequest } from "@/src/domain/models/OrgSubscriptions";
import orgSubscriptionService from "@/src/domain/services/OrgSubscriptionService";
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
  const orgSubscription = await orgSubscriptionService.getOrgSubscription(
    authorizationContext
  );
  return NextResponse.json(orgSubscription);
}

async function postHandler(
  req: NextRequest,
  context: { authorizationContext: AuthorizationContext }
) {
  const { authorizationContext } = context;

  const requestPayload: CreateManageSubscriptionSessionRequest =
    await req.json();

  const manageSubscriptionSession =
    await orgSubscriptionService.createManageSubscriptionSession(
      authorizationContext,
      requestPayload
    );
  return NextResponse.json(manageSubscriptionSession);
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.ORG_SUBSCRIPTIONS,
    SecuredAction.READ,
    [SecuredResourceAccessLevel.ORGANIZATION],
    getHandler
  )
);

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.ORG_SUBSCRIPTIONS,
    SecuredAction.WRITE,
    [SecuredResourceAccessLevel.ORGANIZATION],
    postHandler
  )
);
