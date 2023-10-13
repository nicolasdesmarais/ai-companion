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

  const companion = await prismadb.companion.findUnique({
    where: {
      id: params.chatId,
    },
    include: {
      messages: {
        orderBy: {
          createdAt: "asc",
        },
        where: {
          userId,
        },
      },
      _count: {
        select: {
          messages: true,
        },
      },
    },
  });

  if (!companion) {
    return redirect("/");
  }

  let companions = await prismadb.companion.findMany({
    where: {
      NOT: {
        id: params.chatId,
      },
      AND: [
        {
          messages: {
            some: {
              userId: userId,
            },
          },
        },
      ],
    },
  });
  companions = [companion, ...companions].sort((a, b) => {
    const nameA = a.name.toUpperCase();
    const nameB = b.name.toUpperCase();
    if (nameA < nameB) {
      return -1;
    }
    if (nameA > nameB) {
      return 1;
    }

    return 0;
  });

  return (
    <div className="flex h-full">
      <ChatList companions={companions} />
      <ChatClient companion={companion} />
    </div>
  );
};

export default ChatIdPage;
