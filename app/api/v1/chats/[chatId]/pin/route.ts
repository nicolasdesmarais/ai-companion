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

    const chat = await prismadb.chat.update({
      where: {
        id: chatId,
      },
      data: {
        pinPosition: 1,
      },
    });

    if (!chat) {
      return new NextResponse("Chat not found", { status: 404 });
    }

    return NextResponse.json(chat);
  } catch (error) {
    console.error("[PUT v1/chats/[chatId]/pin]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
