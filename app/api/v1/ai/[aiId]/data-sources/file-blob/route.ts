import { FileUploadDataSourceInput } from "@/src/adapter-out/knowledge/file-upload/types/FileUploadDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { DataSourceRefreshPeriod, DataSourceType } from "@prisma/client";
import { handleUpload, type HandleUploadBody } from "@vercel/blob/client";
import { NextResponse } from "next/server";

async function postHandler(
  request: Request,
  { params: { aiId } }: { params: { aiId: string } }
): Promise<NextResponse> {
  const body = (await request.json()) as HandleUploadBody;

  const jsonResponse = await handleUpload({
    body,
    request,
    onBeforeGenerateToken: async (pathname: string) => {
      const authorizationContext = getUserAuthorizationContext();
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
        aiId,
        blob.pathname,
        DataSourceType.FILE_UPLOAD,
        DataSourceRefreshPeriod.NEVER,
        input
      );
    },
  });

  return NextResponse.json(jsonResponse);
}

export const POST = withErrorHandler(postHandler);
