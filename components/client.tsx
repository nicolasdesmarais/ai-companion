"use client";

import { useCompletion } from "ai/react";
import { useRouter } from "next/navigation";
import { FormEvent, useEffect, useState } from "react";

import { ChatForm } from "@/components/chat-form";
import { ChatHeader } from "@/components/chat-header";
import { ChatMessages } from "@/components/chat-messages";
import { useToast } from "@/components/ui/use-toast";
import { AIDetailDto } from "@/src/domain/models/AI";
import { ChatDetailDto, ChatMessageDto } from "@/src/domain/models/Chats";
import { Role } from "@prisma/client";
import axios from "axios";

interface ChatClientProps {
  ai: AIDetailDto | null;
  chat: ChatDetailDto;
  canEditAi: boolean;
  canApproveAi: boolean;
}

export const ChatClient = ({
  ai,
  chat,
  canEditAi,
  canApproveAi,
}: ChatClientProps) => {
  const router = useRouter();
  const [messages, setMessages] = useState<ChatMessageDto[]>(chat.messages);
  const [streaming, setStreaming] = useState<boolean>(false);
  const { toast } = useToast();

  useEffect(() => {
    const touch = async () => {
      await axios.put(`/api/v1/chats/${chat.id}/touch`);
    };
    touch();
  }, [chat.id]);

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
    <div className="flex flex-col h-full w-full space-y-2 shrink">
      <ChatHeader
        ai={ai}
        chat={chat}
        canEditAi={canEditAi}
        canApproveAi={canApproveAi}
      />
      <ChatMessages
        ai={ai}
        isLoading={isLoading && !stream}
        messages={messages}
        stream={stream}
        canEditAi={canEditAi}
      />
      <ChatForm
        isLoading={!ai || isLoading}
        input={input}
        handleInputChange={handleInputChange}
        onSubmit={onSubmit}
      />
    </div>
  );
};
