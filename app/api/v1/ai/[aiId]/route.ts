import { NextResponse } from "next/server";

import { UpdateAIRequest } from "@/src/domain/ports/api/AIApi";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";

async function patchHandler(
  req: Request,
  context: { params: { aiId: string }; orgId: string; userId: string }
) {
  const { params, orgId, userId } = context;

  if (!params.aiId) {
    return new NextResponse("AI ID required", { status: 400 });
  }

  const body: UpdateAIRequest = await req.json();
  const ai = await aiService.updateAI(orgId, userId, params.aiId, body);

  return NextResponse.json(ai);
}

async function deleteHandler(
  request: Request,
  context: { params: { aiId: string }; orgId: string; userId: string }
) {
  const { params, orgId, userId } = context;

  await aiService.deleteAI(orgId, userId, params.aiId);

  return new NextResponse(null, { status: 204 });
}

export const PATCH = withErrorHandler(
  withAuthorization(
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    patchHandler
  )
);

export const DELETE = withErrorHandler(
  withAuthorization(
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    deleteHandler
  )
);
