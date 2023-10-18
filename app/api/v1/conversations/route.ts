import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import prismadb from "@/lib/prismadb";

export async function GET(req: Request) {
  try {
    const { userId } = await auth();
    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }
    const conversations = await prismadb.conversation.findMany({
      where: {
        userId: userId,
        isDeleted: false,
      },
      include: {
        companion: true,
      },
    });
    return NextResponse.json(conversations);
  } catch (error) {
    console.log("[GET v1/groups]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
