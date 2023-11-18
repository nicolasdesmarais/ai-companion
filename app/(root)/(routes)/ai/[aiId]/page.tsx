import prismadb from "@/src/lib/prismadb";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

interface ChatIdPageProps {
  params: {
    aiId: string;
  };
}
const ChatIdPage = async ({ params }: ChatIdPageProps) => {
  const { userId } = auth();

  if (!userId) {
    return redirectToSignIn();
  }

  const ai = await prismadb.aI.findUnique({
    where: {
      id: params.aiId,
    },
    include: {
      chats: {
        where: {
          userId: userId,
          isDeleted: false,
        },
        orderBy: {
          updatedAt: "desc",
        },
      },
      _count: {
        select: {
          messages: true,
        },
      },
    },
  });

  if (!ai) {
    return redirect("/");
  }

  if (ai.chats.length === 0) {
    const conversation = await prismadb.chat.create({
      data: {
        aiId: params.aiId,
        name: ai.name,
        userId: userId,
      },
    });
    if (ai._count.messages > 0) {
      // add legacy messages to new conversation
      await prismadb.message.updateMany({
        where: {
          aiId: params.aiId,
          userId: userId,
        },
        data: {
          conversationId: conversation.id,
        },
      });
    }
    return redirect(`/chat/${conversation.id}`);
  } else {
    return redirect(`/chat/${ai.chats[0].id}`);
  }
};

export default ChatIdPage;
