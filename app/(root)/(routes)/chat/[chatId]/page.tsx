import { AIProfile } from "@/components/ai-profile";
import { ChatList } from "@/components/chat-list";
import { ChatClient } from "@/components/client";
import { ResizePanel } from "@/components/resize-panel";
import aiService from "@/src/domain/services/AIService";
import chatService from "@/src/domain/services/ChatService";
import { AISecurityService } from "@/src/security/services/AISecurityService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

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

  const canEditAi =
    ai !== null && AISecurityService.canUpdateAI(authorizationContext, ai);

  return (
    <div className="flex h-full">
      <ResizePanel
        name="chat-list-resize-panel"
        initial={360}
        min={80}
        max={600}
        className="hidden md:flex"
      >
        <ChatList />
      </ResizePanel>
      <ChatClient ai={ai} chat={chat} canEditAi={canEditAi} />
      {ai && <AIProfile ai={ai} />}
    </div>
  );
};

export default ChatIdPage;
