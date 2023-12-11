import prismadb from "@/src/lib/prismadb";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

async function putHandler(
  req: Request,
  context: { params: { chatId: string }; orgId: string; userId: string }
) {
  const { params, orgId, userId } = context;
  const chatId = params.chatId;

  const chat = await prismadb.chat.update({
    where: {
      id: chatId,
    },
    data: {
      isDeleted: true,
    },
    include: {
      ai: true,
    },
  });

  if (!chat) {
    return new NextResponse("Conversation not found", { status: 404 });
  }

  const newChat = await prismadb.chat.create({
    data: {
      orgId,
      userId,
      name: chat.name,
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
