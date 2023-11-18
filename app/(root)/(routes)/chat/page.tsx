import prismadb from "@/src/lib/prismadb";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

const ChatPage = async () => {
  const { userId } = auth();

  if (!userId) {
    return redirectToSignIn();
  }

  const conversations = await prismadb.chat.findMany({
    where: {
      userId,
      isDeleted: false,
    },
    orderBy: {
      updatedAt: "desc",
    },
    take: 1,
  });

  if (!conversations.length) {
    return redirect("/");
  }

  return redirect(`/chat/${conversations[0].id}`);
};

export default ChatPage;
