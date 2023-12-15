import { ListDataSourcesResponse } from "@/src/adapter-in/api/DataSourcesApi";
import { DataSourceDto } from "@/src/domain/models/DataSources";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

async function getHandler(
  req: Request,
  context: { authorizationContext: AuthorizationContext }
): Promise<NextResponse<ListDataSourcesResponse>> {
  const { authorizationContext } = context;

  const dataSources: DataSourceDto[] = await dataSourceService.listDataSources(
    authorizationContext
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
