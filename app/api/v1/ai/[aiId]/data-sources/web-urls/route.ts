import { WebUrlDataSourceInput } from "@/src/adapter-out/knowledge/web-urls/types/WebUrlDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceRefreshPeriod, DataSourceType } from "@prisma/client";
import { NextResponse } from "next/server";

async function postHandler(
  req: Request,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  const body = await req.json();
  const { urls, dataRefreshPeriod } = body;

  const dataSources = [];
  for (const url of urls) {
    const input: WebUrlDataSourceInput = {
      url,
    };

    const dataSource = await aiService.createDataSourceAndAddToAI(
      authorizationContext,
      params.aiId,
      url,
      DataSourceType.WEB_URL,
      dataRefreshPeriod,
      input
    );

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
