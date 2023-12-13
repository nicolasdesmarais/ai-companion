import { CreateApiDataSourceRequest } from "@/src/domain/ports/api/DataSourcesApi";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceType } from "@prisma/client";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

async function postHandler(
  request: NextRequest,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
): Promise<NextResponse> {
  const { params, authorizationContext } = context;

  const body: CreateApiDataSourceRequest = await request.json();

  const dataSource = await aiService.createAIDataSource(
    authorizationContext,
    params.aiId,
    body.name,
    DataSourceType.API,
    body
  );

  return NextResponse.json(dataSource, { status: 201 });
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    postHandler
  )
);
