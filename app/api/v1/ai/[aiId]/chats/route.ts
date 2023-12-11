import chatService from "@/src/domain/services/ChatService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

async function getHandler(
  request: NextRequest,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  const chatsResponse = await chatService.getAIChats(
    authorizationContext,
    params.aiId
  );
  return NextResponse.json(chatsResponse);
}

async function postHandler(
  request: NextRequest,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  const chat = await chatService.createChat(authorizationContext, params.aiId);
  return NextResponse.json(chat, { status: 201 });
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.CHATS,
    SecuredAction.READ,
    [SecuredResourceAccessLevel.SELF],
    getHandler
  )
);

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.CHATS,
    SecuredAction.WRITE,
    [SecuredResourceAccessLevel.SELF],
    postHandler
  )
);
