import { CreateChatRequest } from "@/src/domain/ports/api/ChatsApi";
import chatService from "@/src/domain/services/ChatService";
import { AuthorizationScope } from "@/src/domain/types/AuthorizationContext";
import { rateLimit } from "@/src/lib/rate-limit";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { StreamingTextResponse } from "ai";
import { NextResponse } from "next/server";

export const maxDuration = 300;

async function postHandler(
  request: Request,
  context: { params: { chatId: string }; orgId: string; userId: string }
) {
  const { params, userId } = context;
  const chatId = params.chatId;

  const chatRequest: CreateChatRequest = await request.json();

  const identifier = request.url + "-" + userId;
  const { success } = await rateLimit(identifier);

  if (!success) {
    return new NextResponse("Rate limit exceeded", { status: 429 });
  }

  const chatResponse = await chatService.postToChat(
    chatId,
    userId,
    chatRequest
  );

  if (chatResponse.isStream) {
    return new StreamingTextResponse(chatResponse.response as ReadableStream);
  }

  return NextResponse.json(chatResponse.response as string);
}

async function deleteHandler(
  request: Request,
  context: { params: { chatId: string }; orgId: string; userId: string }
) {
  const { params, userId } = context;

  chatService.deleteChat(params.chatId, userId);

  return new NextResponse(null, { status: 204 });
}

export const POST = withErrorHandler(
  withAuthorization(AuthorizationScope.CHATS_WRITE, postHandler)
);

export const DELETE = withErrorHandler(
  withAuthorization(AuthorizationScope.CHATS_WRITE, deleteHandler)
);
