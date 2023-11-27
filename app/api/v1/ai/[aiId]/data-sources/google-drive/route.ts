import { GoogleDriveDataSourceInput } from "@/src/adapter/knowledge/google-drive/types/GoogleDriveDataSourceInput";
import {
  BadRequestError,
  EntityNotFoundError,
} from "@/src/domain/errors/Errors";
import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { CreateGoogleDriveKnowledgeRequest } from "@/src/ports/api/GoogleDriveApi";
import { auth } from "@clerk/nextjs";
import { DataSourceType } from "@prisma/client";
import { NextResponse } from "next/server";

export const maxDuration = 300;

export async function POST(
  req: Request,
  { params }: { params: { aiId: string } }
) {
  const authentication = await auth();
  const userId = authentication?.userId;
  const orgId = authentication?.orgId;
  if (!userId || !orgId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const ai = await aiService.findAIById(params.aiId);
  if (!ai) {
    return new NextResponse("AI not found", { status: 404 });
  }

  const body: CreateGoogleDriveKnowledgeRequest = await req.json();

  try {
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
  } catch (e) {
    console.log(e);
    if (e instanceof EntityNotFoundError) {
      return new NextResponse(e.message, { status: 404 });
    }
    if (e instanceof BadRequestError) {
      return new NextResponse(e.message, { status: 400 });
    }

    return new NextResponse(e.message, { status: 500 });
  }
}
