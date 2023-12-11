import { AIProfile } from "@/components/ai-profile";
import aiService from "@/src/domain/services/AIService";
import chatService from "@/src/domain/services/ChatService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import { ChatList } from "./components/chat-list";
import { ChatClient } from "./components/client";

interface ChatIdPageProps {
  params: {
    chatId: string;
  };
}
const ChatIdPage = async ({ params }: ChatIdPageProps) => {
  const authorizationContext = getUserAuthorizationContext();

  if (!authorizationContext) {
    return redirectToSignIn();
  }

  const chat = await chatService.getChat(authorizationContext, params.chatId);
  const ai = await aiService.findAIForUser(authorizationContext, chat.ai.id);

  if (!chat) {
    return redirect("/");
  }

  return (
    <div className="flex h-full">
      <ChatList />
      <ChatClient ai={ai} chat={chat} />
      <AIProfile ai={ai} />
    </div>
  );
};

export default ChatIdPage;
