import prismadb from "@/src/lib/prismadb";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

const ChatPage = async () => {
  const { userId } = auth();

  if (!userId) {
    return redirectToSignIn();
  }

  const chats = await prismadb.chat.findMany({
    where: {
      userId,
      isDeleted: false,
    },
    orderBy: {
      updatedAt: "desc",
    },
    take: 1,
  });

  if (!chats.length) {
    return redirect("/");
  }

  return redirect(`/chat/${chats[0].id}`);
};

export default ChatPage;
