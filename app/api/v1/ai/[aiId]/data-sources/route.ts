import {
  CreateAIDataSourceRequest,
  CreateAIDataSourceResponse,
} from "@/src/adapter-in/api/AIApi";
import { ListDataSourcesResponse } from "@/src/adapter-in/api/DataSourcesApi";
import { BadRequestError } from "@/src/domain/errors/Errors";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

async function getHandler(
  request: Request,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
): Promise<NextResponse<ListDataSourcesResponse>> {
  const { params, authorizationContext } = context;

  const dataSources = await dataSourceService.listAIDataSources(
    authorizationContext,
    params.aiId
  );

  return NextResponse.json({ data: dataSources });
}

async function postHandler(
  request: Request,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
): Promise<NextResponse<CreateAIDataSourceResponse>> {
  const { params, authorizationContext } = context;

  const { aiId } = params;
  if (!aiId) {
    throw new BadRequestError("aiId is required");
  }

  const body: CreateAIDataSourceRequest = await request.json();
  const { dataSourceId } = body;
  if (!dataSourceId) {
    throw new BadRequestError("dataSourceId is required");
  }

  const aiDataSource = await aiService.addDatasourceToAI(
    authorizationContext,
    aiId,
    dataSourceId
  );

  return new NextResponse(JSON.stringify(aiDataSource), { status: 201 });
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);
