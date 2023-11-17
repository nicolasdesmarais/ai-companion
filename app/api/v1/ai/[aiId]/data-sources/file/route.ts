import { FileUploadDataSourceInput } from "@/src/adapters/knowledge/file-upload/types/FileUploadDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { getAuthorizationContext } from "@/src/lib/authorizationUtils";
import { DataSourceType } from "@prisma/client";
import { put } from "@vercel/blob";
import { writeFile } from "fs/promises";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

/**
 * @swagger
 * /api/v1/ai/{aiId}/data-sources/file:
 *   post:
 *     summary: Adds a file upload data source for the specified AI
 *     parameters:
 *       - in: path
 *         name: aiId
 *         required: true
 *         schema:
 *           type: string
 *         description: The unique identifier for the AI.
 *       - in: query
 *         name: filename
 *         schema:
 *           type: string
 *         required: false
 *         description: The name of the file being uploaded.
 *       - in: query
 *         name: type
 *         schema:
 *           type: string
 *         required: false
 *         description: The content type of the file being uploaded (Text, CSV, Markdown, EPub, Docx, PDF).
 *     requestBody:
 *       content:
 *         multipart/form-data:
 *           schema:
 *             type: object
 *             properties:
 *               file:
 *                 type: string
 *                 format: binary
 *     responses:
 *       '200':
 *         description: File uploaded successfully
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 aiId:
 *                   type: string
 *                 dataSourceId:
 *                   type: string
 *       '400':
 *         description: Bad request
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/Error'
 *       '404':
 *         description: AI not found
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/Error'
 *
 * components:
 *   schemas:
 *     Error:
 *       type: object
 *       properties:
 *         error:
 *           type: string
 */
export async function POST(
  request: NextRequest,
  { params: { aiId } }: { params: { aiId: string } }
): Promise<NextResponse> {
  const authorizationContext = await getAuthorizationContext();
  if (!authorizationContext?.orgId || !authorizationContext?.userId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }
  const { orgId, userId } = authorizationContext;

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
