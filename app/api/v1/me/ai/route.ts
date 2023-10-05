import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { AIService } from "@/domain/services/AIService";

export async function GET(
  request: Request,
  params : ListAIsRequestParams) {
  try {
    const user = await currentUser();
    if (!user?.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const aiService = new AIService();
    const ais = await aiService.findAIsForUser(user.id, params);

    return NextResponse.json(ais);
  } catch (error) {
    console.log("Error on [GET /v1/me/ai]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};

