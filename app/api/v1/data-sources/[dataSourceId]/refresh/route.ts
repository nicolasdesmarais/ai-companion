import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

export const maxDuration = 300;

async function putHandler(
  request: Request,
  context: {
    params: { dataSourceId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  await dataSourceManagementService.refreshDataSourceAsUser(
    authorizationContext,
    params.dataSourceId
  );
  return new NextResponse("Accepted", { status: 202 });
}

export const PUT = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    putHandler
  )
);
