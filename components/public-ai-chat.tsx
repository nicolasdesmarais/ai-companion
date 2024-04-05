import aiService from "@/src/domain/services/AIService";
import { Menu } from "lucide-react";
import Link from "next/link";
import { redirect } from "next/navigation";
import { PublicChatClient } from "./public-chat-client";
import { PublicSidebar } from "./public-sidebar";

interface Props {
  aiId: string;
}

export const PublicAiChat = async ({ aiId }: Props) => {
  try {
    const ais = await aiService.findPublicAIs();
    const ai = await aiService.findPublicAIById(aiId);

    const aiLink = `https://appdirect.ai/public/ai/${aiId}`;
    // This is a workaround to fix the issue with LinkedIn not rendering preview
    fetch(
      `https://www.linkedin.com/post-inspector/inspect/${encodeURIComponent(
        aiLink
      )}`
    );

    if (!ai) {
      throw new Error("AI not found");
    }

    return (
      <div className="h-full">
        <div className="absolute pt-7 pl-5 md:hidden pr-4 z-50">
          <Link href="/signup" className="flex">
            <Menu />
          </Link>
        </div>
        <div className="hidden md:flex h-full w-20 flex-col fixed inset-y-0 z-40">
          <PublicSidebar isChat={true} ais={ais} />
        </div>
        <main className="md:pl-20 md:pt-0 h-full">
          <PublicChatClient ai={ai} ais={ais} />
        </main>
      </div>
    );
  } catch (e) {
    console.error("[PUBLIC CHAT page]", e);
    return redirect("/public");
  }
};
