"use client";

import { AI } from "@prisma/client";
import { ElementRef, useEffect, useRef, useState } from "react";

import { ChatMessage, ChatMessageProps } from "@/components/chat-message";

interface ChatMessagesProps {
  messages: ChatMessageProps[];
  isLoading: boolean;
  ai: AI;
}

export const ChatMessages = ({
  messages = [],
  isLoading,
  ai,
}: ChatMessagesProps) => {
  const scrollRef = useRef<ElementRef<"div">>(null);

  const [fakeLoading, setFakeLoading] = useState(
    messages.length === 0 ? true : false
  );

  useEffect(() => {
    const timeout = setTimeout(() => {
      setFakeLoading(false);
    }, 1000);

    return () => {
      clearTimeout(timeout);
    };
  }, []);

  useEffect(() => {
    scrollRef?.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages.length]);

  return (
    <div className="flex-1 overflow-y-auto px-4">
      <ChatMessage
        isLoading={fakeLoading}
        src={ai.src}
        role="system"
        content={`Hello, I am ${ai.name}, ${ai.description}`}
      />
      {messages.map((message) => (
        <ChatMessage
          key={message.content}
          src={ai.src}
          content={message.content}
          role={message.role}
        />
      ))}
      {isLoading && <ChatMessage src={ai.src} role="system" isLoading />}
      <div ref={scrollRef} />
    </div>
  );
};
