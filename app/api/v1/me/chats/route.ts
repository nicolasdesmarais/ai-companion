import chatService from "@/src/domain/services/ChatService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

async function getHandler(
  request: NextRequest,
  context: { orgId: string; userId: string }
) {
  const { userId, orgId } = context;

  const chatsResponse = await chatService.getUserChats(userId, orgId);
  return NextResponse.json(chatsResponse);
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.CHATS,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);
