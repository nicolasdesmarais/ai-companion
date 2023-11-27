import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

import { UpdateAIRequest } from "@/src/domain/ports/api/AIApi";
import aiService from "@/src/domain/services/AIService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";

export async function patchHandler(
  req: Request,
  { params }: { params: { aiId: string } }
) {
  if (!params.aiId) {
    return new NextResponse("AI ID required", { status: 400 });
  }

  const { orgId, userId } = await auth();
  if (!userId || !orgId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }
  const body: UpdateAIRequest = await req.json();
  const ai = await aiService.updateAI(orgId, userId, params.aiId, body);

  return NextResponse.json(ai);
}

async function deleteHandler(
  request: Request,
  { params }: { params: { aiId: string } }
) {
  const { userId, orgId } = await auth();

  if (!userId || !orgId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  await aiService.deleteAI(orgId, userId, params.aiId);

  return new NextResponse(null, { status: 204 });
}

export const PATCH = withErrorHandler(patchHandler);
export const DELETE = withErrorHandler(deleteHandler);
