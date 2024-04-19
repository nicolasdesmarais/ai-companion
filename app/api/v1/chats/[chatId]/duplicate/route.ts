import prismadb from "@/src/lib/prismadb";
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
  const { orgId, userId } = authorizationContext;
  const chatId = params.chatId;

  const chat = await prismadb.chat.findUnique({
    where: {
      id: chatId,
    },
    include: {
      ai: true,
    },
  });

  if (!chat) {
    return new NextResponse("Chat not found", { status: 404 });
  }

  const chatCount = await prismadb.chat.count({
    where: {
      userId: userId,
      aiId: chat.ai.id,
      isDeleted: false,
    },
  });

  const newChat = await prismadb.chat.create({
    data: {
      orgId: orgId,
      userId: userId,
      name: chatCount > 0 ? `(${chatCount + 1})` : "",
      aiId: chat.ai.id,
    },
  });

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
