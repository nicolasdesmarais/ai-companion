"use client";

import { ChatForm } from "@/components/chat-form";
import { ChatHeader } from "@/components/chat-header";
import { ChatMessages } from "@/components/chat-messages";
import { AIDetailDto } from "@/src/domain/models/AI";

interface ChatClientProps {
  ai: AIDetailDto | null;
}

export const PublicChatClient = ({ ai }: ChatClientProps) => {
  const canEditAi = false;
  const canApproveAi = false;
  return (
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
  );
};
