import { ListDataSourcesResponse } from "@/src/adapter-in/api/DataSourcesApi";
import {
  DataSourceDto,
  DataSourceFilter,
  DataSourceOrderBy,
  DataSourceOrderByDirection,
  DataSourceOrderByField,
} from "@/src/domain/models/DataSources";
import dataSourceViewingService from "@/src/domain/services/DataSourceViewingService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceType } from "@prisma/client";
import { NextRequest, NextResponse } from "next/server";
async function getHandler(
  req: NextRequest,
  context: { authorizationContext: AuthorizationContext }
): Promise<NextResponse<ListDataSourcesResponse>> {
  const { authorizationContext } = context;

  const { searchParams } = new URL(req.url);
  const search = searchParams.get("search") || undefined;
  const type = searchParams.get("type");
  const orderByParam = searchParams.get("orderBy");

  let orderBy: DataSourceOrderBy | undefined;
  if (orderByParam) {
    const direction = orderByParam.startsWith("-")
      ? DataSourceOrderByDirection.DESC
      : DataSourceOrderByDirection.ASC;
    const field = orderByParam.substring(1) as DataSourceOrderByField;
    orderBy = { field, direction };
  }

  const filter: DataSourceFilter = {
    search,
    type: type ? (type as DataSourceType) : undefined,
    orderBy,
  };

  const dataSources: DataSourceDto[] =
    await dataSourceViewingService.listDataSourcesByLevel(
      authorizationContext,
      SecuredResourceAccessLevel.SELF,
      filter
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
