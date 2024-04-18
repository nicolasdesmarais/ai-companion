import chatService from "@/src/domain/services/ChatService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

async function putHandler(
  req: Request,
  context: {
    params: { chatId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;
  const chatId = params.chatId;

  const newChat = await chatService.resetChat(authorizationContext, chatId);

  return NextResponse.json(newChat);
}

export const PUT = withErrorHandler(
  withAuthorization(
    SecuredResourceType.CHATS,
    SecuredAction.WRITE,
    [SecuredResourceAccessLevel.SELF],
    putHandler
  )
);
