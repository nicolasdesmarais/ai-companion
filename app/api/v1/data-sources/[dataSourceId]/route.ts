import dataSourceService from "@/src/domain/services/DataSourceService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { NextResponse } from "next/server";

export const maxDuration = 300;

async function deleteHandler(
  request: Request,
  context: {
    params: { dataSourceId: string };
    orgId: string;
    userId: string;
  }
) {
  const { params, orgId, userId } = context;

  await dataSourceService.deleteDataSource(orgId, userId, params.dataSourceId);
  return new NextResponse(null, { status: 204 });
}

export const DELETE = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    deleteHandler
  )
);
