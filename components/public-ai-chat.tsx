import aiService from "@/src/domain/services/AIService";
import { redirect } from "next/navigation";
import { PublicChatClient } from "./public-chat-client";
import { PublicSidebar } from "./public-sidebar";

interface Props {
  aiId: string;
}

export const PublicAiChat = async ({ aiId }: Props) => {
  try {
    const ai = await aiService.findPublicAI(aiId);
    return (
      <div className="h-full">
        <div className="hidden md:flex h-full w-20 flex-col fixed inset-y-0 z-40">
          <PublicSidebar isChat={true} />
        </div>
        <main className="md:pl-20 pt-20 md:pt-0 h-full">
          <PublicChatClient ai={ai} />
        </main>
      </div>
    );
  } catch (e) {
    console.error("[PUBLIC CHAT page]", e);
    return redirect("/public");
  }
};
