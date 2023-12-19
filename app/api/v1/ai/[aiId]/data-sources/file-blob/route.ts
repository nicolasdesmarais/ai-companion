import { FileUploadDataSourceInput } from "@/src/adapter-out/knowledge/file-upload/types/FileUploadDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceType } from "@prisma/client";
import { handleUpload, type HandleUploadBody } from "@vercel/blob/client";
import { NextResponse } from "next/server";

async function postHandler(
  request: Request,
  context: {
    params: { aiId: string };
    authorizationContext: AuthorizationContext;
  }
): Promise<NextResponse> {
  const { params } = context;
  const body = (await request.json()) as HandleUploadBody;

  const jsonResponse = await handleUpload({
    body,
    request,
    onBeforeGenerateToken: async (pathname: string) => {
      const { authorizationContext } = context;
      return {
        tokenPayload: JSON.stringify({
          authorizationContext,
        }),
      };
    },
    onUploadCompleted: async ({ blob, tokenPayload }) => {
      if (!tokenPayload) {
        throw new Error("Missing tokenPayload");
      }
      const { authorizationContext } = JSON.parse(tokenPayload);

      const input: FileUploadDataSourceInput = {
        filename: blob.pathname,
        mimetype: blob.contentType,
        blobUrl: blob.url,
      };

      await aiService.createDataSourceAndAddToAI(
        authorizationContext,
        params.aiId,
        blob.pathname,
        DataSourceType.FILE_UPLOAD,
        input
      );
    },
  });

  return NextResponse.json(jsonResponse);
}

export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    postHandler
  )
);
