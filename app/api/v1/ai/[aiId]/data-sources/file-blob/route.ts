import { FileUploadDataSourceInput } from "@/src/adapter-out/knowledge/file-upload/types/FileUploadDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { auth } from "@clerk/nextjs";
import { DataSourceType } from "@prisma/client";
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
      const { userId, orgId } = await auth();
      return {
        tokenPayload: JSON.stringify({
          orgId,
          userId,
        }),
      };
    },
    onUploadCompleted: async ({ blob, tokenPayload }) => {
      if (!tokenPayload) {
        throw new Error("Missing tokenPayload");
      }
      const { orgId, userId } = JSON.parse(tokenPayload);

      const input: FileUploadDataSourceInput = {
        filename: blob.pathname,
        mimetype: blob.contentType,
        blobUrl: blob.url,
      };
      const dataSourceId = await dataSourceService.createDataSource(
        orgId,
        userId,
        blob.pathname,
        DataSourceType.FILE_UPLOAD,
        input
      );
      await aiService.createAIDataSource(aiId, dataSourceId);
    },
  });

  return NextResponse.json(jsonResponse);
}

export const POST = withErrorHandler(postHandler);
