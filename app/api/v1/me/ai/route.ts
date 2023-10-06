import { AIService } from "@/domain/services/AIService";
import { ListAIsRequestParams, ListAIsRequestScope } from "@/domain/services/dtos/ListAIsRequestParams";
import { currentUser } from "@clerk/nextjs";
import { NextRequest, NextResponse } from "next/server";

export async function GET(
  req: NextRequest): Promise<NextResponse> {
  try {

    const user = await currentUser();
    if (!user?.id) {
      return NextResponse.json("Unauthorized", { status: 401 });
    }

    const { searchParams } = new URL(req.url);

    const scopeParam = searchParams.get('scope');
    let scope: ListAIsRequestScope;
    if (scopeParam === null) {
      scope = ListAIsRequestScope.ALL;
    } else  {
      if (!Object.values(ListAIsRequestScope).includes(scopeParam as ListAIsRequestScope)) {
        return NextResponse.json("Invalid scope", { status: 400 });
      }
      scope = ListAIsRequestScope[scopeParam as keyof typeof ListAIsRequestScope];
    }

    const requestParams: ListAIsRequestParams = {
      scope: scope,
      search: searchParams.get('search'),
    }

    const aiService = new AIService();
    const ais = await aiService.findAIsForUser(user.id, requestParams);

    return NextResponse.json(ais);
  } catch (error) {
    console.log("Error on [GET /v1/me/ai]", error);
    return NextResponse.json("Internal Error", { status: 500 });
  }
};
