import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import prismadb from "@/src/lib/prismadb";

export const maxDuration = 300;

export async function POST(
  request: Request,
  {
    params: { aiId, conversationId },
  }: { params: { aiId: string; conversationId: string } }
) {
  try {
    const { answer } = await request.json();
    const user = await currentUser();

    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const conversation = await prismadb.conversation.update({
      where: {
        id: conversationId,
      },
      data: {
        messages: {
          create: {
            content: answer,
            role: "system",
            userId: user.id,
            aiId: aiId,
          },
        },
      },
    });

    if (!conversation) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    return NextResponse.json("", { status: 200 });
  } catch (error) {
    if (error.response?.data?.error?.message) {
      console.error("[CHAT] answer", error.response.data.error.message);
      return new NextResponse(error.response.data.error.message, {
        status: 500,
      });
    }
    console.error("[CHAT] answer", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
