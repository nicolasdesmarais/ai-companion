import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { AuthorizationScope } from "@/src/domain/types/AuthorizationContext";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { CreateApiDataSourceRequest } from "@/src/ports/api/DataSourcesApi";
import { DataSourceType } from "@prisma/client";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

async function postHandler(
  request: NextRequest,
  context: { params: { aiId: string }; orgId: string; userId: string }
): Promise<NextResponse> {
  const { params, orgId, userId } = context;

  const body: CreateApiDataSourceRequest = await request.json();

  const dataSourceId = await dataSourceService.createDataSource(
    orgId,
    userId,
    body.name,
    DataSourceType.API,
    body
  );
  const dataSource = await aiService.createAIDataSource(
    params.aiId,
    dataSourceId
  );

  return NextResponse.json(dataSource, { status: 201 });
}

export const POST = withErrorHandler(
  withAuthorization(AuthorizationScope.DATA_SOURCES_WRITE, postHandler)
);
