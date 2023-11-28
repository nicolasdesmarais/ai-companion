import { CreateAIRequest } from "@/src/domain/ports/api/AIApi";
import aiService from "@/src/domain/services/AIService";
import EmailUtils from "@/src/lib/emailUtils";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

async function postHandler(req: Request) {
  const body: CreateAIRequest = await req.json();
  const authentication = await auth();
  const { orgId } = authentication;
  const user = await currentUser();

  if (!user?.id || !orgId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const emailAddress = EmailUtils.getUserPrimaryEmailAddress(user);
  const createAiRequest = {
    ...body,
    userName: emailAddress ?? user.firstName ?? user.username ?? "user",
  };

  const ai = await aiService.createAI(orgId, user.id, createAiRequest);

  return NextResponse.json(ai);
}

export const POST = withErrorHandler(postHandler);
