import { UpdateDataSourceRequest } from "@/src/adapter-in/api/DataSourcesApi";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

export const maxDuration = 300;

async function deleteHandler(
  request: Request,
  context: {
    params: { dataSourceId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  await dataSourceManagementService.requestDeleteDataSource(
    authorizationContext,
    params.dataSourceId
  );
  return new NextResponse(null, { status: 204 });
}

async function patchHandler(
  request: Request,
  context: {
    params: { dataSourceId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  if (!params.dataSourceId) {
    return new NextResponse("Data Source ID required", { status: 400 });
  }

  const body: UpdateDataSourceRequest = await request.json();
  const dataSource = await dataSourceManagementService.updateDataSource(
    authorizationContext,
    params.dataSourceId,
    body
  );

  return NextResponse.json(dataSource);
}

export const PATCH = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    patchHandler
  )
);

export const DELETE = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    deleteHandler
  )
);
