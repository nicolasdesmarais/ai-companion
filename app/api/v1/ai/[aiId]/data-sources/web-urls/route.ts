import { WebUrlDataSourceInput } from "@/src/adapter/knowledge/web-urls/types/WebUrlDataSourceInput";
import {
  BadRequestError,
  EntityNotFoundError,
} from "@/src/domain/errors/Errors";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { auth } from "@clerk/nextjs";
import { DataSourceType } from "@prisma/client";
import { NextResponse } from "next/server";

export async function POST(
  req: Request,
  { params }: { params: { aiId: string } }
) {
  console.log("POST /api/v1/ai/[aiId]/knowledge/web-urls");
  const authentication = await auth();
  const userId = authentication?.userId;
  const orgId = authentication?.orgId;

  if (!userId || !orgId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const ai = await aiService.findAIById(params.aiId);
  if (!ai) {
    return new NextResponse("AI not found", { status: 404 });
  }

  const body = await req.json();
  const { urls } = body;

  try {
    const dataSources = [];
    for (const url of urls) {
      const input: WebUrlDataSourceInput = {
        url,
      };

      const dataSourceId = await dataSourceService.createDataSource(
        orgId,
        userId,
        url,
        DataSourceType.WEB_URL,
        input
      );

      const dataSource = await aiService.createAIDataSource(
        ai.id,
        dataSourceId
      );
      dataSources.push(dataSource);
    }

    return NextResponse.json(dataSources, { status: 201 });
  } catch (e) {
    console.log(e);
    if (e instanceof EntityNotFoundError) {
      return NextResponse.json({ folders: [], knowledgeIds: [] });
    }
    if (e instanceof BadRequestError) {
      return new NextResponse(e.message, { status: 400 });
    }

    return new NextResponse(e.message, { status: 500 });
  }
}
