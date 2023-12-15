import { ListDataSourcesResponse } from "@/src/adapter-in/api/DataSourcesApi";
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

  const dataSources = await dataSourceService.getAIDataSources(
    authorizationContext,
    params.aiId
  );

  return NextResponse.json({ data: dataSources });
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);
