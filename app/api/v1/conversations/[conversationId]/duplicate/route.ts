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

    const conversation = await prismadb.conversation.findUnique({
      where: {
        id: conversationId,
      },
      include: {
        companion: true,
      },
    });

    if (!conversation) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    const conversationCount = await prismadb.conversation.count({
      where: {
        userId: userId,
        companionId: conversation.companion.id,
      },
    });

    const newConversation = await prismadb.conversation.create({
      data: {
        userId: userId,
        name: `${conversation.companion.name} (${conversationCount + 1})`,
        companionId: conversation.companion.id,
      },
    });

    return NextResponse.json(newConversation);
  } catch (error) {
    console.error("[PUT v1/conversation/[conversationId]/duplicate]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
