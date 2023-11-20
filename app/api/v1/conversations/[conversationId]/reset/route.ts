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

    const chat = await prismadb.chat.update({
      where: {
        id: conversationId,
      },
      data: {
        isDeleted: true,
      },
      include: {
        ai: true,
      },
    });

    if (!chat) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    const newConversation = await prismadb.chat.create({
      data: {
        userId: userId,
        name: chat.name,
        aiId: chat.ai.id,
      },
    });

    return NextResponse.json(newConversation);
  } catch (error) {
    console.error("[PUT v1/conversation/[conversationId]/reset]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
