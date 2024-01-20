import chatService from "@/src/domain/services/ChatService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

async function postHandler(
  request: NextRequest,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;
  const { tokensUsed, prompt, messages } = await request.json();
  const chat = await chatService.getKnowledge(
    { aiId: params.aiId, date: "", prompt, messages },
    authorizationContext.userId,
    tokensUsed
  );
  return NextResponse.json(chat, { status: 201 });
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.CHATS,
    SecuredAction.READ,
    [SecuredResourceAccessLevel.SELF],
    postHandler
  )
);
