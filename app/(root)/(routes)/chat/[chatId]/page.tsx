import prismadb from "@/src/lib/prismadb";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import { ChatList } from "./components/chat-list";
import { ChatClient } from "./components/client";

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
      ai: true,
      _count: {
        select: {
          messages: true,
        },
      },
    },
  });

  if (!conversation || conversation.isDeleted) {
    return redirect("/");
  }

  return (
    <div className="flex h-full">
      <ChatList />
      <ChatClient conversation={conversation} />
    </div>
  );
};

export default ChatIdPage;
