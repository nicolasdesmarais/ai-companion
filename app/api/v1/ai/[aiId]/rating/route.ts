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

    await aiService.rateAi(userId, aiId, rating, review);

    return NextResponse.json("OK", { status: 200 });
  } catch (error) {
    console.log("Error on [PUT /v1/ai/[aiId]/rating]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
}

export async function GET(
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

    const rating = await aiService.getUserAiRating(userId, aiId);

    return NextResponse.json(rating, { status: 200 });
  } catch (error) {
    console.log("Error on [GET /v1/ai/[aiId]/rating]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
}
