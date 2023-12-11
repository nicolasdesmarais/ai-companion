import aiService from "@/src/domain/services/AIService";
import chatService from "@/src/domain/services/ChatService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

interface ChatIdPageProps {
  params: {
    aiId: string;
  };
}
const ChatIdPage = async ({ params }: ChatIdPageProps) => {
  const authorizationContext = getUserAuthorizationContext();

  if (!authorizationContext) {
    return redirectToSignIn();
  }

  const ai = await aiService.findAIForUser(authorizationContext, params.aiId);
  if (!ai) {
    return redirect("/");
  }

  const aiChats = await chatService.getAIChats(
    authorizationContext,
    params.aiId
  );
  if (aiChats.data.length === 0) {
    const chat = await chatService.createChat(
      authorizationContext,
      params.aiId
    );
    return redirect(`/chat/${chat.id}`);
  } else {
    return redirect(`/chat/${aiChats.data[0].id}`);
  }
};

export default ChatIdPage;
