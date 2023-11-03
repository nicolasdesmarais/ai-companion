import { FileUploadDataSourceInput } from "@/src/adapters/knowledge/file-upload/types/FileUploadDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { auth } from "@clerk/nextjs";
import { DataSourceType } from "@prisma/client";
import { writeFile } from "fs/promises";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

export async function POST(
  request: NextRequest,
  { params: { aiId } }: { params: { aiId: string } }
): Promise<NextResponse> {
  const authentication = await auth();
  const userId = authentication?.userId;
  const orgId = authentication.orgId;

  if (!userId || !orgId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const { searchParams } = new URL(request.url);
  const filename = searchParams.get("filename");
  const type = searchParams.get("type");

  if (!filename || !type) {
    return new NextResponse("Missing required fields", { status: 400 });
  }

  if (!request.body) {
    return NextResponse.json("Missing file", { status: 400 });
  }

  try {
    const data = await request.formData();
    const file: File | null = data.get("file") as unknown as File;

    const bytes = await file.arrayBuffer();
    const buffer = Buffer.from(bytes);
    const filepath = `/tmp/${file.name}`;
    await writeFile(filepath, buffer);

    const input: FileUploadDataSourceInput = {
      filename,
      mimetype: type,
      filepath,
    };
    const dataSourceId = await dataSourceService.createDataSource(
      orgId,
      userId,
      filename,
      DataSourceType.FILE_UPLOAD,
      input
    );
    const dataSource = await aiService.createAIDataSource(aiId, dataSourceId);

    return NextResponse.json(dataSource, { status: 201 });
  } catch (error) {
    if (error.response?.data?.error?.message) {
      return new NextResponse(error.response.data.error.message, {
        status: 422,
      });
    }
    console.log("[KNOWLEDGE_ERROR]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
