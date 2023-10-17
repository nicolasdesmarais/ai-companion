import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import prismadb from "@/lib/prismadb";

const ChatPage = async () => {
  const { userId } = auth();

  if (!userId) {
    return redirectToSignIn();
  }

  const conversations = await prismadb.conversation.findMany({
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
  console.log(conversations);

  return redirect(`/chat/${conversations[0].id}`);
};

export default ChatPage;
