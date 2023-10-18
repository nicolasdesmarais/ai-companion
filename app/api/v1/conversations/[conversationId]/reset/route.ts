import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import prismadb from "@/lib/prismadb";

export async function PUT(
  req: Request,
  { params: { conversationId } }: { params: { conversationId: string } }
) {
  try {
    const { userId } = await auth();
    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const conversation = await prismadb.conversation.update({
      where: {
        id: conversationId,
      },
      data: {
        isDeleted: true,
      },
      include: {
        companion: true,
      },
    });

    if (!conversation) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    const newConversation = await prismadb.conversation.create({
      data: {
        userId: userId,
        name: conversation.name,
        companionId: conversation.companion.id,
      },
    });

    return NextResponse.json(newConversation);
  } catch (error) {
    console.error("[PUT v1/conversation/[conversationId]/reset]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
