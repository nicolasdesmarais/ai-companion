import { GoogleDriveDataSourceInput } from "@/src/adapter-out/knowledge/google-drive/types/GoogleDriveDataSourceInput";
import { CreateGoogleDriveKnowledgeRequest } from "@/src/domain/ports/api/GoogleDriveApi";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceType } from "@prisma/client";
import { NextResponse } from "next/server";

export const maxDuration = 300;

async function postHandler(
  req: Request,
  context: { params: { aiId: string }; orgId: string; userId: string }
) {
  const { params, orgId, userId } = context;

  const ai = await aiService.findAIById(params.aiId);
  if (!ai) {
    return new NextResponse("AI not found", { status: 404 });
  }

  const body: CreateGoogleDriveKnowledgeRequest = await req.json();

  const input: GoogleDriveDataSourceInput = {
    oauthTokenId: body.oauthTokenId,
    fileId: body.fileId,
  };
  const dataSourceId = await dataSourceService.createDataSource(
    orgId,
    userId,
    body.filename,
    DataSourceType.GOOGLE_DRIVE,
    input
  );

  await aiService.createAIDataSource(ai.id, dataSourceId);

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
