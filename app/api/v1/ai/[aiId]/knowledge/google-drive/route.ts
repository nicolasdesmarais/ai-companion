import { AIService } from "@/domain/services/AIService";
import { GoogleDriveLoader } from "@/domain/services/knowledge/GoogleDriveLoader";
import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function POST(
  req: Request,
  { params }: { params: { aiId: string } }
) {
  const user = await currentUser();
  if (!user) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const body = await req.json();

  const userId = user.id;
  const { folderName } = body;

  const googleDriveLoader = new GoogleDriveLoader();
  const knowledgeIds = await googleDriveLoader.loadFolder(userId, folderName);
  const aiService = new AIService();
  await aiService.createKnowledgeAI(params.aiId, knowledgeIds);

  return new NextResponse("", { status: 201 });
}
