import aiService from "@/src/domain/services/AIService";
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
    const { rating, review } = await req.json();

    await aiService.rateAi(orgId, userId, aiId, rating, review);

    return NextResponse.json("", { status: 200 });
  } catch (error) {
    console.log("Error on [PUT /v1/ai/[aiId]/share]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
}
