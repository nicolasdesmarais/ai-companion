import { FileUploadDataSourceInput } from "@/src/adapters/knowledge/file-upload/types/FileUploadDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { AuthorizationScope } from "@/src/domain/types/AuthorizationContext";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { DataSourceType } from "@prisma/client";
import { put } from "@vercel/blob";
import { writeFile } from "fs/promises";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

export async function postHandler(
  request: NextRequest,
  context: { params: { aiId: string }; orgId: string; userId: string }
): Promise<NextResponse> {
  const { params, orgId, userId } = context;

  if (!request.body) {
    return NextResponse.json("Missing file", { status: 400 });
  }

  const data = await request.formData();
  const file: File | null = data.get("file") as unknown as File;
  const filename = file.name;
  const type = file.type;

  const bytes = await file.arrayBuffer();
  const buffer = Buffer.from(bytes);
  const filepath = `/tmp/${file.name}`;
  await writeFile(filepath, buffer);

  const blob = await put(filename, file, {
    access: "public",
  });

  const input: FileUploadDataSourceInput = {
    filename,
    mimetype: type,
    filepath,
    blobUrl: blob.url,
  };
  const dataSourceId = await dataSourceService.createDataSource(
    orgId,
    userId,
    filename,
    DataSourceType.FILE_UPLOAD,
    input
  );
  const dataSource = await aiService.createAIDataSource(
    params.aiId,
    dataSourceId
  );

  return NextResponse.json(dataSource, { status: 201 });
}

export const POST = withErrorHandler(
  withAuthorization(AuthorizationScope.DATA_SOURCES_WRITE, postHandler)
);
