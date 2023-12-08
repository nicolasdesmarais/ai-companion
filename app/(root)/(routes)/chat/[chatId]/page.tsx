import prismadb from "@/src/lib/prismadb";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import { ChatList } from "./components/chat-list";
import { ChatClient } from "./components/client";
import { AIProfile } from "@/components/ai-profile";

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

  const chat = await prismadb.chat.findUnique({
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

  if (!chat || chat.isDeleted) {
    return redirect("/");
  }

  const ratingResult = await prismadb.aIRating.aggregate({
    _avg: {
      rating: true,
    },
    _count: {
      rating: true,
    },
    where: {
      aiId: chat.ai.id,
    },
  });
  const rating = {
    averageRating: ratingResult._avg.rating,
    ratingCount: ratingResult._count.rating,
  };
  return (
    <div className="flex h-full">
      <ChatList />
      <ChatClient chat={chat} rating={rating} />
      <AIProfile ai={chat.ai} rating={rating} />
    </div>
  );
};

export default ChatIdPage;
