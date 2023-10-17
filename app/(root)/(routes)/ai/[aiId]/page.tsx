import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import prismadb from "@/lib/prismadb";

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

  const ai = await prismadb.companion.findUnique({
    where: {
      id: params.aiId,
    },
    include: {
      conversations: {
        where: {
          messages: {
            some: {
              userId: userId,
            },
          },
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

  if (ai.conversations.length === 0) {
    const conversation = await prismadb.conversation.create({
      data: {
        companionId: params.aiId,
        name: ai.name,
      },
    });
    if (ai._count.messages > 0) {
      // add legacy messages to new conversation
      await prismadb.message.updateMany({
        where: {
          companionId: params.aiId,
          userId: userId,
        },
        data: {
          conversationId: conversation.id,
        },
      });
    }
    return redirect(`/chat/${conversation.id}`);
  } else {
    return redirect(`/chat/${ai.conversations[0].id}`);
  }
};

export default ChatIdPage;
