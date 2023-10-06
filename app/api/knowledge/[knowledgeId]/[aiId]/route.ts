import { NextResponse } from 'next/server';
import { currentUser } from "@clerk/nextjs";
import prismadb from "@/lib/prismadb";
import { MemoryManager } from "@/lib/memory";

export async function DELETE(
  request: Request,
  { params }: { params: { knowledgeId: string, aiId: string } }
) {
  try {
    const user = await currentUser();
    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorDelete(params.knowledgeId);

    await prismadb.knowledgeAI.delete({
      where: {
        knowledgeId_companionId: {
          knowledgeId: params.knowledgeId,
          companionId: params.aiId
        }
      }
    });

    const knowledge = await prismadb.knowledge.delete({
      where: {
        id: params.knowledgeId
      }
    });

    return NextResponse.json(knowledge);
  } catch (error) {
    console.log("[KNOWLEDGE_DELETE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};