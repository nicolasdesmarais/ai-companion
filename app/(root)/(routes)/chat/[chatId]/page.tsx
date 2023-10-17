import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import prismadb from "@/lib/prismadb";
import { ChatClient } from "./components/client";
import { ChatList } from "./components/chat-list";

interface ChatIdPageProps {
  params: {
    chatId: string;
  };
}
const ChatIdPage = async ({ params }: ChatIdPageProps) => {
  const { userId } = auth();

  if (!userId) {
    return redirectToSignIn();
  }

  const conversation = await prismadb.conversation.findUnique({
    where: {
      id: params.chatId,
    },
    include: {
      messages: {
        orderBy: {
          createdAt: "asc",
        },
      },
      companion: true,
      _count: {
        select: {
          messages: true,
        },
      },
    },
  });

  if (!conversation) {
    return redirect("/");
  }

  const conversations = await prismadb.conversation.findMany({
    where: {
      messages: {
        some: {
          userId: userId,
        },
      },
    },
    include: {
      companion: true,
    },
  });

  return (
    <div className="flex h-full">
      <ChatList initialConversations={conversations} />
      <ChatClient conversation={conversation} />
    </div>
  );
};

export default ChatIdPage;
