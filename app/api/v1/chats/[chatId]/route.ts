import { CreateChatRequest } from "@/src/adapter-in/api/ChatsApi";
import { ChatDetailDto } from "@/src/domain/models/Chats";
import chatService from "@/src/domain/services/ChatService";
import { tokenBucketRateLimit } from "@/src/lib/rate-limit";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { StreamingTextResponse } from "ai";
import { NextResponse } from "next/server";

export const maxDuration = 300;
const TOKEN_BUCKET_REFILL_RATE = 10;
const TOKEN_BUCKET_INTERVAL = "10 s";
const TOKEN_BUCKET_MAX_TOKENS = 100;

async function getHandler(
  request: Request,
  context: {
    params: { chatId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;
  const chatId = params.chatId;

  const chat: ChatDetailDto = await chatService.getChat(
    authorizationContext,
    chatId
  );
  return NextResponse.json(chat);
}

async function postHandler(
  request: Request,
  context: {
    params: { chatId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;
  const chatId = params.chatId;
  const { orgId } = authorizationContext;

  const chatRequest: CreateChatRequest = await request.json();

  const identifier = request.url + "-" + orgId;
  const isWithinRateLimit = await tokenBucketRateLimit(
    identifier,
    TOKEN_BUCKET_REFILL_RATE,
    TOKEN_BUCKET_INTERVAL,
    TOKEN_BUCKET_MAX_TOKENS,
    1
  );

  if (!isWithinRateLimit) {
    return new NextResponse("Rate limit exceeded", { status: 429 });
  }

  const chatResponse = await chatService.postToChat(
    authorizationContext,
    chatId,
    chatRequest
  );

  if (chatResponse.isStream) {
    return new StreamingTextResponse(chatResponse.response as ReadableStream);
  }

  return new NextResponse(chatResponse.response as string, { status: 200 });
}

async function deleteHandler(
  request: Request,
  context: { params: { chatId: string }; orgId: string; userId: string }
) {
  const { params, userId } = context;

  await chatService.deleteChat(params.chatId, userId);

  return new NextResponse(null, { status: 204 });
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

export const DELETE = withErrorHandler(
  withAuthorization(
    SecuredResourceType.CHATS,
    SecuredAction.WRITE,
    [SecuredResourceAccessLevel.SELF],
    deleteHandler
  )
);
