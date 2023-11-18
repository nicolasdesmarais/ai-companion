import prismadb from "@/src/lib/prismadb";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function PUT(
  req: Request,
  { params: { conversationId } }: { params: { conversationId: string } }
) {
  try {
    const { userId } = await auth();
    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const conversation = await prismadb.chat.findUnique({
      where: {
        id: conversationId,
      },
      include: {
        ai: true,
      },
    });

    if (!conversation) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    const conversationCount = await prismadb.chat.count({
      where: {
        userId: userId,
        aiId: conversation.ai.id,
      },
    });

    const newConversation = await prismadb.chat.create({
      data: {
        userId: userId,
        name: `${conversation.ai.name} (${conversationCount + 1})`,
        aiId: conversation.ai.id,
      },
    });

    return NextResponse.json(newConversation);
  } catch (error) {
    console.error("[PUT v1/conversation/[conversationId]/duplicate]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
