import { BadRequestError, EntityNotFoundError } from "@/domain/errors/Errors";
import { AIService } from "@/domain/services/AIService";
import { ApifyService } from "@/domain/services/ApifyService";
import prismadb from "@/lib/prismadb";
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
  const { urls } = body;

  try {
    const apifyService = new ApifyService();
    urls.forEach(async (url: string) => {
      const knowledge = await prismadb.knowledge.create({
        data: {
          userId: userId,
          name: url,
          type: "URL",
        },
      });

      await aiService.createKnowledgeAI(params.aiId, [knowledge.id]);
      await apifyService.createWebUrlKnowledge(knowledge.id, url);
    });

    return new NextResponse("", { status: 201 });
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
