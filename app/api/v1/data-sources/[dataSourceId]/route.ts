import dataSourceService from "@/src/domain/services/DataSourceService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationScope } from "@/src/security/models/AuthorizationContext";
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
  withAuthorization(AuthorizationScope.DATA_SOURCES_WRITE, deleteHandler)
);
