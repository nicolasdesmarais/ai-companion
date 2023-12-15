import { NextResponse } from "next/server";

import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { AIDetailDto, UpdateAIRequest } from "@/src/domain/models/AIApi";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";

async function getHandler(
  req: Request,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  if (!params.aiId) {
    return new NextResponse("AI ID required", { status: 400 });
  }

  const ai: AIDetailDto | null = await aiService.findAIForUser(
    authorizationContext,
    params.aiId
  );

  if (!ai) {
    throw new EntityNotFoundError(`AI with ID ${params.aiId} not found`);
  }

  return NextResponse.json(ai);
}

async function patchHandler(
  req: Request,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  if (!params.aiId) {
    return new NextResponse("AI ID required", { status: 400 });
  }

  const body: UpdateAIRequest = await req.json();
  const ai = await aiService.updateAI(authorizationContext, params.aiId, body);

  return NextResponse.json(ai);
}

async function deleteHandler(
  request: Request,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  await aiService.deleteAI(authorizationContext, params.aiId);

  return new NextResponse(null, { status: 204 });
}
export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.AI,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);

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
