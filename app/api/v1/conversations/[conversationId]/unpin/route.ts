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
        pinPosition: null,
      },
    });

    if (!chat) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    return NextResponse.json(chat);
  } catch (error) {
    console.error("[PUT v1/conversation/[conversationId]/unpin]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
