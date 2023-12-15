import { CreateAIRequest } from "@/src/adapter-in/api/AIApi";
import aiService from "@/src/domain/services/AIService";
import EmailUtils from "@/src/lib/emailUtils";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import {
  AuthorizationContext,
  AuthorizationContextType,
} from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { clerkClient, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

async function postHandler(
  req: Request,
  context: {
    authorizationContext: AuthorizationContext;
  }
) {
  const { authorizationContext } = context;
  const body: CreateAIRequest = await req.json();

  let user;
  if (authorizationContext.type === AuthorizationContextType.USER) {
    user = await currentUser();
  } else {
    const { userId } = authorizationContext;
    user = await clerkClient.users.getUser(userId);
  }

  if (!user) {
    throw new Error("User not found");
  }

  const emailAddress = EmailUtils.getUserPrimaryEmailAddress(user);
  const createAiRequest = {
    ...body,
    userName: emailAddress ?? user.firstName ?? user.username ?? "user",
  };

  const ai = await aiService.createAI(authorizationContext, createAiRequest);

  return NextResponse.json(ai);
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    postHandler
  )
);
