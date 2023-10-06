import { currentUser } from "@clerk/nextjs";
import { NextRequest, NextResponse  } from "next/server";
import { AIService } from "@/domain/services/AIService";
import { ListAIsRequestParams, ListAIsRequestScope } from "@/domain/services/dtos/ListAIsRequestParams";

export async function GET(
  req: NextRequest): Promise<NextResponse> {
  try {

    const user = await currentUser();
    if (!user?.id) {
      return NextResponse.json("Unauthorized", { status: 401 });
    }

    const { searchParams } = new URL(req.url);

    const requestParams: ListAIsRequestParams = {
      scope: searchParams.get('scope') as ListAIsRequestScope
    }


    const aiService = new AIService();
    const ais = await aiService.findAIsForUser(user.id, requestParams);

    return NextResponse.json(ais);
  } catch (error) {
    console.log("Error on [GET /v1/me/ai]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
};

