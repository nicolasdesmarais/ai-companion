import { AIService } from "@/domain/services/AIService";
import { ShareAIRequest } from "@/domain/types/ShareAIRequest";
import { currentUser } from "@clerk/nextjs";
import { NextRequest, NextResponse } from "next/server";

export async function PUT(
  req: NextRequest,
  { params }: { params: { companionId: string } }
): Promise<NextResponse> {
  try {
    const user = await currentUser();
    if (!user?.id) {
      return NextResponse.json("Unauthorized", { status: 401 });
    }

    const aiId = params.companionId;
    const shareAiRequest: ShareAIRequest = await req.json();

    const aiService = new AIService();
    const ai = await aiService.findAIById(aiId);
    if (ai === null) {
      return NextResponse.json("Not Found", { status: 404 });
    }

    if (ai.userId !== user.id) {
      return NextResponse.json("Forbidden", { status: 403 });
    }

    aiService.shareAi(aiId, shareAiRequest);

    return NextResponse.json("", { status: 201 });
  } catch (error) {
    console.log("Error on [GET /v1/me/ai]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
}
