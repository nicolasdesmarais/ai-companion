"use client";

import { AI, Chat, Message } from "@prisma/client";
import { useCompletion } from "ai/react";
import { FormEvent, ReactNode, useState } from "react";
import { ChatForm } from "@/components/chat-form";
import { ChatMessageProps } from "@/components/chat-message";
import { ChatMessages } from "@/components/chat-messages";
import { useToast } from "@/components/ui/use-toast";

type Props = {
  ai: AI;
  className?: string;
  actions: ReactNode;
};

export const TestChat = ({ ai, className, actions }: Props) => {
  const [messages, setMessages] = useState<ChatMessageProps[]>([]);
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
      const systemMessage: ChatMessageProps = {
        role: "system",
        content: completion,
      };

      setMessages((current) => [...current, systemMessage]);
      setInput("");
    },
  });

  const onSubmit = (e: FormEvent<HTMLFormElement>) => {
    const userMessage: ChatMessageProps = {
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
