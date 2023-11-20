import prismadb from "@/src/lib/prismadb";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function DELETE(
  request: Request,
  {
    params: { conversationId },
  }: {
    params: { conversationId: string };
  }
) {
  try {
    const { userId } = await auth();
    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const chat = await prismadb.chat.findUnique({
      where: {
        id: conversationId,
      },
      include: {
        ai: true,
      },
    });

    if (!chat) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    if (chat.userId !== userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const deletedConversation = await prismadb.chat.update({
      where: {
        id: conversationId,
      },
      data: {
        isDeleted: true,
      },
    });

    return NextResponse.json(deletedConversation);
  } catch (error) {
    console.error("[DELETE v1/conversation/[conversationId]]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
