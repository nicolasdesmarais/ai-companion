import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import prismadb from "@/src/lib/prismadb";

export async function GET(
  request: Request,
  { params }: { params: { aiId: string } }
) {
  try {
    const user = await currentUser();
    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const knowledge = await prismadb.knowledgeAI.findMany({
      where: {
        companionId: params.aiId,
      },
      include: {
        knowledge: true,
      },
    });

    return NextResponse.json(knowledge);
  } catch (error) {
    console.log("[KNOWLEDGE_GET]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
