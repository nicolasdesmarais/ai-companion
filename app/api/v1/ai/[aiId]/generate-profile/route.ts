import { ShareAIRequest } from "@/src/domain/ports/api/ShareAIRequest";
import aiService from "@/src/domain/services/AIService";
import { auth } from "@clerk/nextjs";
import { NextRequest, NextResponse } from "next/server";

export async function PUT(
  req: NextRequest,
  { params }: { params: { aiId: string } }
): Promise<NextResponse> {
  try {
    const authentication = await auth();
    const { userId } = authentication;
    if (!userId) {
      return NextResponse.json("Unauthorized", { status: 401 });
    }

    const aiId = params.aiId;

    const aiProfile = await aiService.generateAIProfile(userId, aiId);

    return NextResponse.json(aiProfile, { status: 200 });
  } catch (error) {
    console.log("Error on [PUT /v1/ai/[aiId]/generate-profile]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
}
