import aiService from "@/src/domain/services/AIService";
import chatService from "@/src/domain/services/ChatService";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

interface ChatIdPageProps {
  params: {
    aiId: string;
  };
}
const ChatIdPage = async ({ params }: ChatIdPageProps) => {
  const { orgId, userId } = auth();

  if (!orgId || !userId) {
    return redirectToSignIn();
  }

  const ai = await aiService.findAIForUser(orgId, userId, params.aiId);
  if (!ai) {
    return redirect("/");
  }

  const aiChats = await chatService.getAIChats(params.aiId, userId);
  if (aiChats.data.length === 0) {
    const chat = await chatService.createChat(orgId, userId, params.aiId);
    return redirect(`/chat/${chat.id}`);
  } else {
    return redirect(`/chat/${aiChats.data[0].id}`);
  }
};

export default ChatIdPage;
