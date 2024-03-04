"use client";

import { AIProfile } from "@/components/ai-profile";
import { ChatMessages } from "@/components/chat-messages";
import { ResizePanel } from "@/components/resize-panel";
import { AIDetailDto } from "@/src/domain/models/AI";
import { PublicChatForm } from "./public-chat-form";
import { PublicChatHeader } from "./public-chat-header";
import { PublicChatList } from "./public-chat-list";

interface ChatClientProps {
  ai: AIDetailDto | null;
  ais: AIDetailDto[];
}

export const PublicChatClient = ({ ai, ais }: ChatClientProps) => {
  return (
    <div className="flex h-full">
      <ResizePanel
        name="chat-list-resize-panel"
        initial={360}
        min={80}
        max={600}
        className="hidden md:flex"
      >
        <PublicChatList ais={ais} ai={ai} />
      </ResizePanel>
      <div className="flex flex-col h-full w-full space-y-2 shrink">
        <PublicChatHeader ai={ai} chat={{ messages: [], ai } as any} />
        <ChatMessages
          ai={ai}
          isLoading={false}
          messages={[]}
          stream={""}
          canEditAi={false}
        />
        <PublicChatForm isLoading={!ai} />
      </div>
      {ai && <AIProfile ai={ai} />}
    </div>
  );
};
