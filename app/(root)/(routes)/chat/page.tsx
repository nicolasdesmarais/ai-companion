import chatService from "@/src/domain/services/ChatService";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

const ChatPage = async () => {
  const { userId, orgId } = auth();

  if (!userId || !orgId) {
    return redirectToSignIn();
  }

  const chats = await chatService.getUserChats(userId, orgId);

  if (!chats.data.length) {
    return redirect("/");
  }

  return redirect(`/chat/${chats.data[0].id}`);
};

export default ChatPage;
