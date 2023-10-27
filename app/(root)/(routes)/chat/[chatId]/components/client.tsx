"use client";

import { AI, Conversation, Message } from "@prisma/client";
import { useCompletion } from "ai/react";
import { useRouter } from "next/navigation";
import { FormEvent, useState } from "react";

import { ChatForm } from "@/components/chat-form";
import { ChatHeader } from "@/components/chat-header";
import { ChatMessageProps } from "@/components/chat-message";
import { ChatMessages } from "@/components/chat-messages";
import { useToast } from "@/components/ui/use-toast";

interface ChatClientProps {
  conversation: Conversation & {
    messages: Message[];
    ai: AI;
    _count: {
      messages: number;
    };
  };
}

export const ChatClient = ({ conversation }: ChatClientProps) => {
  const router = useRouter();
  const [messages, setMessages] = useState<ChatMessageProps[]>(
    conversation.messages
  );
  const { toast } = useToast();

  const { input, isLoading, handleInputChange, handleSubmit, setInput } =
    useCompletion({
      api: `/api/chat/${conversation.ai.id}/${conversation.id}`,
      onError: (err) => {
        toast({
          variant: "destructive",
          description: err.message,
          duration: 60000,
        });
      },
      onFinish(_prompt, completion) {
        const systemMessage: ChatMessageProps = {
          role: "system",
          content: completion,
        };

        setMessages((current) => [...current, systemMessage]);
        setInput("");

        router.refresh();
      },
    });

  const onSubmit = (e: FormEvent<HTMLFormElement>) => {
    const userMessage: ChatMessageProps = {
      role: "user",
      content: input,
    };

    setMessages((current) => [...current, userMessage]);

    handleSubmit(e);
  };

  return (
    <div className="flex flex-col h-full w-full space-y-2 ml-1">
      <ChatHeader conversation={conversation} />
      <ChatMessages
        ai={conversation.ai}
        isLoading={isLoading}
        messages={messages}
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
