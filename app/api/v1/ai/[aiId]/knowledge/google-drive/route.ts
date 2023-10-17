import { BadRequestError, EntityNotFoundError } from "@/domain/errors/Errors";
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

  const aiService = new AIService();
  const ai = await aiService.findAIById(params.aiId);
  if (!ai) {
    return new NextResponse("AI not found", { status: 404 });
  }

  const userId = user.id;
  const body = await req.json();
  const { folderName } = body;

  try {
    const googleDriveLoader = new GoogleDriveLoader();
    const loadFolderResponse = await googleDriveLoader.loadFolder(
      userId,
      folderName
    );
    const aiService = new AIService();
    const response = await aiService.createKnowledgeAI(
      params.aiId,
      loadFolderResponse.knowledgeIds
    );

    return NextResponse.json(loadFolderResponse);
  } catch (e) {
    console.log(e);
    if (e instanceof EntityNotFoundError) {
      return NextResponse.json({ folders: [], knowledgeIds: [] });
    }
    if (e instanceof BadRequestError) {
      return new NextResponse(e.message, { status: 400 });
    }

    return new NextResponse(e.message, { status: 500 });
  }
}
