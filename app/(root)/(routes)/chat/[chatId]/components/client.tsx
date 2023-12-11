"use client";

import { useCompletion } from "ai/react";
import { useRouter } from "next/navigation";
import { FormEvent, useState } from "react";

import { ChatForm } from "@/components/chat-form";
import { ChatHeader } from "@/components/chat-header";
import { ChatMessages } from "@/components/chat-messages";
import { useToast } from "@/components/ui/use-toast";
import { AIDto } from "@/src/domain/ports/api/AIApi";
import { ChatDetailDto, ChatMessageDto } from "@/src/domain/ports/api/ChatsApi";

interface ChatClientProps {
  ai: AIDto;
  chat: ChatDetailDto;
}

export const ChatClient = ({ ai, chat }: ChatClientProps) => {
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
        id: "",
        createdAt: new Date(),
        updatedAt: new Date(),
        role: "system",
        content: completion,
      };

      setMessages((current) => [...current, systemMessage]);
      setInput("");

      router.refresh();
    },
  });

  const onSubmit = (e: FormEvent<HTMLFormElement>) => {
    const userMessage: ChatMessageDto = {
      id: "",
      createdAt: new Date(),
      updatedAt: new Date(),
      role: "user",
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
      <ChatHeader ai={ai} chat={chat} />
      <ChatMessages
        ai={ai}
        isLoading={isLoading && !stream}
        messages={chat.messages}
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
