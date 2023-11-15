import aiService from "@/src/domain/services/AIService";
import { ShareAIRequest } from "@/src/domain/types/ShareAIRequest";
import { auth } from "@clerk/nextjs";
import { NextRequest, NextResponse } from "next/server";

export async function PUT(
  req: NextRequest,
  { params }: { params: { aiId: string } }
): Promise<NextResponse> {
  try {
    const authentication = await auth();
    const { orgId, userId } = authentication;
    if (!orgId || !userId) {
      return NextResponse.json("Unauthorized", { status: 401 });
    }

    const aiId = params.aiId;
    const shareAiRequest: ShareAIRequest = await req.json();

    await aiService.shareAi(orgId, userId, aiId, shareAiRequest);

    return NextResponse.json("", { status: 200 });
  } catch (error) {
    console.log("Error on [PUT /v1/me/ai]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
}
