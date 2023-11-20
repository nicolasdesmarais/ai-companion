import prismadb from "@/src/lib/prismadb";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function PUT(
  req: Request,
  { params: { chatId } }: { params: { chatId: string } }
) {
  try {
    const { userId } = await auth();
    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const chat = await prismadb.chat.findUnique({
      where: {
        id: chatId,
      },
      include: {
        ai: true,
      },
    });

    if (!chat) {
      return new NextResponse("Chat not found", { status: 404 });
    }

    const chatCount = await prismadb.chat.count({
      where: {
        userId: userId,
        aiId: chat.ai.id,
      },
    });

    const newChat = await prismadb.chat.create({
      data: {
        userId: userId,
        name: `${chat.ai.name} (${chatCount + 1})`,
        aiId: chat.ai.id,
      },
    });

    return NextResponse.json(newChat);
  } catch (error) {
    console.error("[PUT v1/chats/[chatId]/duplicate]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
