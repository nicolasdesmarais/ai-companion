import chatService from "@/src/domain/services/ChatService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationScope } from "@/src/security/models/AuthorizationContext";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

async function getHandler(
  request: NextRequest,
  context: { orgId: string; userId: string }
) {
  const { userId } = context;

  const chatsResponse = await chatService.getUserChats(userId);
  return NextResponse.json(chatsResponse);
}

export const GET = withErrorHandler(
  withAuthorization(AuthorizationScope.CHATS_READ, getHandler)
);
