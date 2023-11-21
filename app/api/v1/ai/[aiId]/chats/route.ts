import chatService from "@/src/domain/services/ChatService";
import { AuthorizationScope } from "@/src/domain/types/AuthorizationContext";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

async function getHandler(
  request: NextRequest,
  context: { params: { aiId: string }; orgId: string; userId: string }
) {
  const { params, userId } = context;

  const chatsResponse = await chatService.getAIChats(params.aiId, userId);
  return NextResponse.json(chatsResponse);
}

async function postHandler(
  request: NextRequest,
  context: { params: { aiId: string }; orgId: string; userId: string }
) {
  const { params, orgId, userId } = context;

  const chat = await chatService.createChat(orgId, userId, params.aiId);
  return NextResponse.json(chat, { status: 201 });
}

export const GET = withErrorHandler(
  withAuthorization(AuthorizationScope.CHATS_READ, getHandler)
);

export const POST = withErrorHandler(
  withAuthorization(AuthorizationScope.CHATS_WRITE, postHandler)
);
