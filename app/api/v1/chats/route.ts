import { getAuthorizationContext } from "@/src/lib/authorizationUtils";
import prismadb from "@/src/lib/prismadb";
import { NextResponse } from "next/server";

export async function GET(req: Request) {
  try {
    const authorizationContext = await getAuthorizationContext();
    if (!authorizationContext?.orgId || !authorizationContext?.userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }
    const { userId } = authorizationContext;

    const conversations = await prismadb.conversation.findMany({
      where: {
        userId: userId,
        isDeleted: false,
      },
      include: {
        ai: true,
      },
    });
    return NextResponse.json(conversations);
  } catch (error) {
    console.log("[GET v1/chats]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
