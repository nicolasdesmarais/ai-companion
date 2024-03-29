import { CreateApiDataSourceRequest } from "@/src/adapter-in/api/DataSourcesApi";
import { ApiDataSourceInput } from "@/src/adapter-out/knowledge/api/ApiDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import { FileStorageService } from "@/src/domain/services/FileStorageService";
import { createSha256Hash } from "@/src/lib/encryptionUtils";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceRefreshPeriod, DataSourceType } from "@prisma/client";
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

  let body: CreateApiDataSourceRequest;
  try {
    body = await request.json();
  } catch (e) {
    return NextResponse.json("Invalid JSON", { status: 400 });
  }

  if (!body.name || !body.data) {
    return NextResponse.json("Missing name or data", { status: 400 });
  }

  // Convert the data to a buffer and upload it to the blob store
  const jsonData = JSON.stringify(body.data);
  const buffer = Buffer.from(jsonData);
  const hash = createSha256Hash(buffer);

  const blobUrl = await FileStorageService.put(`${body.name}.json`, buffer);

  const dataSourceInput: ApiDataSourceInput = {
    name: body.name,
    blobUrl,
    hash,
  };

  const dataSource = await aiService.createDataSourceAndAddToAI(
    authorizationContext,
    params.aiId,
    body.name,
    DataSourceType.API,
    DataSourceRefreshPeriod.NEVER,
    dataSourceInput
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
