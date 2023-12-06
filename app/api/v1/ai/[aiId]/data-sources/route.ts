import dataSourceService from "@/src/domain/services/DataSourceService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationScope } from "@/src/security/models/AuthorizationContext";
import { NextResponse } from "next/server";

async function getHandler(
  request: Request,
  context: { params: { aiId: string }; orgId: string; userId: string }
) {
  const { params, orgId, userId } = context;

  const dataSources = await dataSourceService.getDataSources(
    orgId,
    userId,
    params.aiId
  );

  return NextResponse.json(dataSources);
}

export const GET = withErrorHandler(
  withAuthorization(AuthorizationScope.DATA_SOURCES_READ, getHandler)
);
