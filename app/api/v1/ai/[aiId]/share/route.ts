import { ShareAIRequest } from "@/src/adapter-in/api/AIApi";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextRequest, NextResponse } from "next/server";

async function putHandler(
  req: NextRequest,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
): Promise<NextResponse> {
  const { params, authorizationContext } = context;

  const aiId = params.aiId;
  const shareAiRequest: ShareAIRequest = await req.json();

  await aiService.shareAi(authorizationContext, aiId, shareAiRequest);

  return NextResponse.json("", { status: 200 });
}

export const PUT = withErrorHandler(
  withAuthorization(
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    putHandler
  )
);
