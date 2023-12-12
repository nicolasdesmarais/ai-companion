"use client";

import { ChatForm } from "@/components/chat-form";
import { ChatMessages } from "@/components/chat-messages";
import { useToast } from "@/components/ui/use-toast";
import { AIDetailDto } from "@/src/domain/ports/api/AIApi";
import { ChatMessageDto } from "@/src/domain/ports/api/ChatsApi";
import { useCompletion } from "ai/react";
import {
  Dispatch,
  FormEvent,
  ReactNode,
  SetStateAction,
  useState,
} from "react";

type Props = {
  ai: AIDetailDto;
  className?: string;
  actions: ReactNode;
  messages: ChatMessageDto[];
  setMessages: Dispatch<SetStateAction<ChatMessageDto[]>>;
};

export const TestChat = ({
  ai,
  className,
  actions,
  messages,
  setMessages,
}: Props) => {
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
    api: `/api/v1/chats/test-chat`,
    body: {
      date: new Date().toLocaleString(),
      aiId: ai.id,
      messages,
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
        role: "system",
        content: completion,
      };

      setMessages((current) => [...current, systemMessage]);
      setInput("");
    },
  });

  const onSubmit = (e: FormEvent<HTMLFormElement>) => {
    const userMessage: ChatMessageDto = {
      createdAt: new Date(),
      updatedAt: new Date(),
      role: "user",
      content: input,
    };

    setMessages((current) => [...current, userMessage]);

    setStreaming(true);
    handleSubmit(e);
    e.stopPropagation();
  };

  let stream = "";
  if (streaming) {
    stream = completion;
  }
  return (
    <div className="flex flex-col h-full w-full space-y-2 ml-1 shrink">
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
      <div className="p-4">{actions}</div>
    </div>
  );
};
