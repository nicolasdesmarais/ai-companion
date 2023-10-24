import { MemoryManager } from "@/src/lib/memory";
import prismadb from "@/src/lib/prismadb";
import { currentUser } from "@clerk/nextjs";
import { Prisma } from "@prisma/client";
import { NextResponse } from "next/server";

export const maxDuration = 300;

export async function DELETE(
  request: Request,
  { params }: { params: { knowledgeId: string; aiId: string } }
) {
  try {
    const user = await currentUser();
    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorDelete(params.knowledgeId);

    await prismadb.knowledgeAI
      .delete({
        where: {
          knowledgeId_companionId: {
            knowledgeId: params.knowledgeId,
            companionId: params.aiId,
          },
        },
      })
      .catch((e) => {
        if (
          (e instanceof Prisma.PrismaClientKnownRequestError &&
            e.code === "P2025") ||
          e.code === "P2016"
        ) {
          // AI is not saved yet
          return;
        }
        throw e;
      });

    const knowledge = await prismadb.knowledge.delete({
      where: {
        id: params.knowledgeId,
      },
    });

    return NextResponse.json(knowledge);
  } catch (error) {
    console.log("[KNOWLEDGE_DELETE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
