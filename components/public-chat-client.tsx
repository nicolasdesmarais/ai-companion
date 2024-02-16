"use client";

import { AIProfile } from "@/components/ai-profile";
import { ChatForm } from "@/components/chat-form";
import { ChatHeader } from "@/components/chat-header";
import { ChatMessages } from "@/components/chat-messages";
import { ResizePanel } from "@/components/resize-panel";
import { AIDetailDto } from "@/src/domain/models/AI";
import { PublicChatList } from "./public-chat-list";

interface ChatClientProps {
  ai: AIDetailDto | null;
  ais: AIDetailDto[];
}

export const PublicChatClient = ({ ai, ais }: ChatClientProps) => {
  const canEditAi = false;
  const canApproveAi = false;
  return (
    <div className="flex h-full">
      <ResizePanel
        name="chat-list-resize-panel"
        initial={360}
        min={80}
        max={600}
        className="hidden md:flex"
      >
        <PublicChatList ais={ais} />
      </ResizePanel>
      <div className="flex flex-col h-full w-full space-y-2 shrink">
        <ChatHeader
          ai={ai}
          chat={{ messages: [], ai } as any}
          canEditAi={canEditAi}
          canApproveAi={canApproveAi}
        />
        <ChatMessages
          ai={ai}
          isLoading={false}
          messages={[]}
          stream={""}
          canEditAi={canEditAi}
        />
        <ChatForm
          isLoading={!ai}
          input={""}
          handleInputChange={() => {}}
          onSubmit={() => {}}
        />
      </div>
      {ai && <AIProfile ai={ai} />}
    </div>
  );
};
