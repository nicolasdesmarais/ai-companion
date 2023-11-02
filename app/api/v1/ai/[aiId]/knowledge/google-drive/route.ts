import { inngest } from "@/src/adapters/inngest/client";
import { GoogleDriveDataSourceInput } from "@/src/adapters/knowledge/google-drive/types/GoogleDriveDataSourceInput";
import aiService from "@/src/domain/services/AIService";
import { CreateGoogleDriveKnowledgeRequest } from "@/src/domain/types/CreateGoogleDriveKnowledgeRequest";
import { auth } from "@clerk/nextjs";
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
  const input: GoogleDriveDataSourceInput = {
    oauthTokenId: body.oauthTokenId,
    fileId: body.fileId,
  };

  await inngest.send({
    name: "google-drive/datasource.creation.requested",
    data: {
      orgId,
      userId,
      aiId: ai.id,
      input,
    },
  });

  return new NextResponse("", { status: 202 });
}
