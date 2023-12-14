"use client";

import { useCompletion } from "ai/react";
import { useRouter } from "next/navigation";
import { FormEvent, useState } from "react";

import { ChatForm } from "@/components/chat-form";
import { ChatHeader } from "@/components/chat-header";
import { ChatMessages } from "@/components/chat-messages";
import { useToast } from "@/components/ui/use-toast";
import { AIDetailDto } from "@/src/domain/ports/api/AIApi";
import { ChatDetailDto, ChatMessageDto } from "@/src/domain/ports/api/ChatsApi";
import { Role } from "@prisma/client";

interface ChatClientProps {
  ai: AIDetailDto | null;
  chat: ChatDetailDto;
  canEditAi: boolean;
}

export const ChatClient = ({ ai, chat, canEditAi }: ChatClientProps) => {
  const router = useRouter();
  const [messages, setMessages] = useState<ChatMessageDto[]>(chat.messages);
  const [streaming, setStreaming] = useState<boolean>(false);
  const { toast } = useToast();

  const {
    completion,
    input,
    isLoading,
    handleInputChange,
    handleSubmit,
    setInput,
  } = useCompletion({
    api: `/api/v1/chats/${chat.id}`,
    body: {
      date: new Date().toLocaleString(),
    },
    onError: (err) => {
      toast({
        variant: "destructive",
        description: err.message,
        duration: 60000,
      });
    },
    onFinish(_prompt, completion) {
      setStreaming(false);
      const systemMessage: ChatMessageDto = {
        createdAt: new Date(),
        updatedAt: new Date(),
        role: Role.system,
        content: completion,
      };

      setMessages((current) => [...current, systemMessage]);
      setInput("");

      router.refresh();
    },
  });

  const onSubmit = (e: FormEvent<HTMLFormElement>) => {
    const userMessage: ChatMessageDto = {
      createdAt: new Date(),
      updatedAt: new Date(),
      role: Role.user,
      content: input,
    };

    setMessages((current) => [...current, userMessage]);

    setStreaming(true);
    handleSubmit(e);
  };

  let stream = "";
  if (streaming) {
    stream = completion;
  }
  return (
    <div className="flex flex-col h-full w-full space-y-2 ml-1 shrink">
      <ChatHeader ai={ai} chat={chat} canEditAi={canEditAi} />
      <ChatMessages
        ai={ai}
        isLoading={isLoading && !stream}
        messages={messages}
        stream={stream}
      />
      <ChatForm
        isLoading={isLoading}
        input={input}
        handleInputChange={handleInputChange}
        onSubmit={onSubmit}
      />
    </div>
  );
};
