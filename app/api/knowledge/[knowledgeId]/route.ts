import { MemoryManager } from "@/src/lib/memory";
import prismadb from "@/src/lib/prismadb";
import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export const maxDuration = 300;

export async function DELETE(
  request: Request,
  { params }: { params: { knowledgeId: string } }
) {
  try {
    const user = await currentUser();
    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorDelete(params.knowledgeId);

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
