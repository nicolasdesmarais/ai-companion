import { CreateGoogleDriveKnowledgeRequest } from "@/src/adapter-in/api/GoogleDriveApi";
import { GoogleDriveDataSourceInput } from "@/src/adapter-out/knowledge/google-drive/types/GoogleDriveDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceRefreshPeriod, DataSourceType } from "@prisma/client";
import { NextResponse } from "next/server";

export const maxDuration = 300;

async function postHandler(
  req: Request,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
) {
  const { params, authorizationContext } = context;

  const { oauthTokenId, fileId, filename, dataRefreshPeriod } =
    await req.json();

  await aiService.createDataSourceAndAddToAI(
    authorizationContext,
    params.aiId,
    filename,
    DataSourceType.ONEDRIVE,
    dataRefreshPeriod ?? DataSourceRefreshPeriod.NEVER,
    { oauthTokenId, fileId }
  );

  return new NextResponse("", { status: 201 });
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    postHandler
  )
);
