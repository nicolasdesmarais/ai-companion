import prismadb from "@/src/lib/prismadb";
import { NextResponse } from "next/server";

import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";

async function putHandler(
  req: Request,
  context: { params: { chatId: string }; orgId: string; userId: string }
) {
  const { params, userId } = context;
  const chatId = params.chatId;
  const chat = await prismadb.chat.update({
    where: {
      id: chatId,
    },
    data: {
      pinPosition: 1,
    },
  });

  if (!chat) {
    return new NextResponse("Chat not found", { status: 404 });
  }

  return NextResponse.json(chat);
}

export const PUT = withErrorHandler(
  withAuthorization(
    SecuredResourceType.CHATS,
    SecuredAction.WRITE,
    [SecuredResourceAccessLevel.SELF],
    putHandler
  )
);
