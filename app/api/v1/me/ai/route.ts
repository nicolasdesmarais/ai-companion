import { AIService } from "@/domain/services/AIService";
import { ListAIsRequestParams, ListAIsRequestScope } from "@/domain/services/dtos/ListAIsRequestParams";
import { auth } from "@clerk/nextjs";
import { NextRequest, NextResponse } from "next/server";

export async function GET(
  req: NextRequest): Promise<NextResponse> {
  try {

    const authorization = await auth();
    const { searchParams } = new URL(req.url);

    const scopeParam = searchParams.get('scope');
    let scope: ListAIsRequestScope | undefined;

    if (!scopeParam) {
      scope = undefined;
    } else if (!Object.values(ListAIsRequestScope).includes(scopeParam as ListAIsRequestScope)) {
      return NextResponse.json("Invalid scope", { status: 400 });
    } else {
      scope = ListAIsRequestScope[scopeParam as keyof typeof ListAIsRequestScope];
    }

    const requestParams: ListAIsRequestParams = {
      scope: scope,
      groupId: searchParams.get('groupId'),
      categoryId: searchParams.get('categoryId'),
      search: searchParams.get('search'),
    }

    const aiService = new AIService();
    const ais = await aiService.findAIsForUser(authorization,requestParams);

    return NextResponse.json(ais);
  } catch (error) {
    console.log("Error on [GET /v1/me/ai]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
};
