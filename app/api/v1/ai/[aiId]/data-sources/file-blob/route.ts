import { FileUploadDataSourceInput } from "@/src/adapters/knowledge/file-upload/types/FileUploadDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { DataSourceType } from "@prisma/client";
import { NextResponse } from "next/server";
import { handleUpload, type HandleUploadBody } from "@vercel/blob/client";
import { knowledgeTypes } from "@/components/knowledge-types";
import { auth } from "@clerk/nextjs";

export async function POST(
  request: Request,
  { params: { aiId } }: { params: { aiId: string } }
): Promise<NextResponse> {
  const body = (await request.json()) as HandleUploadBody;

  try {
    const jsonResponse = await handleUpload({
      body,
      request,
      onBeforeGenerateToken: async (pathname: string) => {
        const { userId, orgId } = await auth();

        const allowedContentTypes = knowledgeTypes.map(
          (type: any) => type.type
        );
        return {
          allowedContentTypes,
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
        try {
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
        } catch (error) {
          throw new Error("Could not update user");
        }
      },
    });

    return NextResponse.json(jsonResponse);
  } catch (error) {
    return NextResponse.json(
      { error: (error as Error).message },
      { status: 400 } // The webhook will retry 5 times waiting for a 200
    );
  }
}
