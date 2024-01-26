import { NextResponse } from "next/server";

import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import chatService from "@/src/domain/services/ChatService";

async function putHandler(
  req: Request,
  context: {
    params: { chatId: string };
    authorizationContext: AuthorizationContext;
    orgId: string;
    userId: string;
  }
) {
  const { params, authorizationContext } = context;
  const chatId = params.chatId;
  const summary = await chatService.summarizeChat(authorizationContext, chatId);

  return NextResponse.json({ summary });
}

export const PUT = withErrorHandler(
  withAuthorization(
    SecuredResourceType.CHATS,
    SecuredAction.WRITE,
    [SecuredResourceAccessLevel.SELF],
    putHandler
  )
);
