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
    api: `/api/v1/ai/${conversation.ai.id}/chats`,
    body: {
      date: new Date().toLocaleTimeString(),
      conversationId: conversation.id,
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

      router.refresh();
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
      <ChatHeader conversation={conversation} />
      <ChatMessages
        ai={conversation.ai}
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
