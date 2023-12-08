import { WebUrlDataSourceInput } from "@/src/adapter-out/knowledge/web-urls/types/WebUrlDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceType } from "@prisma/client";
import { NextResponse } from "next/server";

async function postHandler(
  req: Request,
  context: { params: { aiId: string }; orgId: string; userId: string }
) {
  const { params, orgId, userId } = context;

  const ai = await aiService.findAIById(params.aiId);
  if (!ai) {
    return new NextResponse("AI not found", { status: 404 });
  }

  const body = await req.json();
  const { urls } = body;

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

    const dataSource = await aiService.createAIDataSource(ai.id, dataSourceId);
    dataSources.push(dataSource);
  }

  return NextResponse.json(dataSources, { status: 201 });
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    postHandler
  )
);
