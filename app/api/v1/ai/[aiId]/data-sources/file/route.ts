import { FileUploadDataSourceInput } from "@/src/adapter-out/knowledge/file-upload/types/FileUploadDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { DataSourceType } from "@prisma/client";
import { put } from "@vercel/blob";
import crypto from "crypto";
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

  if (!request.body) {
    return NextResponse.json("Missing file", { status: 400 });
  }

  const data = await request.formData();
  const file: File | null = data.get("file") as unknown as File;
  const filename = file.name;
  const type = file.type;

  const bytes = await file.arrayBuffer();
  const buffer = Buffer.from(bytes);

  // Calculate a hash for the file
  const fileHash = crypto.createHash("sha256").update(buffer).digest("hex");

  const blob = await put(filename, file, {
    access: "public",
  });

  const input: FileUploadDataSourceInput = {
    filename,
    mimetype: type,
    blobUrl: blob.url,
    fileHash,
  };

  const dataSource = await aiService.createDataSourceAndAddToAI(
    authorizationContext,
    params.aiId,
    filename,
    DataSourceType.FILE_UPLOAD,
    input
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
