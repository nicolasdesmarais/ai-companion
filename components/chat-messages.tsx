"use client";

import { ElementRef, useEffect, useRef, useState } from "react";

import { ChatMessage } from "@/components/chat-message";
import { useInspectMessage } from "@/hooks/use-inspect-message";
import { AISummaryDto } from "@/src/domain/models/AI";
import { ChatMessageDto } from "@/src/domain/models/Chats";
import { InspectMessageModal } from "./inspect-message-modal";

interface ChatMessagesProps {
  messages: ChatMessageDto[];
  isLoading: boolean;
  ai: AISummaryDto | null;
  stream: string;
  canEditAi: boolean;
}

export const ChatMessages = ({
  messages = [],
  isLoading,
  ai,
  stream,
  canEditAi,
}: ChatMessagesProps) => {
  const scrollRef = useRef<ElementRef<"div">>(null);

  const [fakeLoading, setFakeLoading] = useState(messages.length === 0);
  const inspectMessageModal = useInspectMessage();

  useEffect(() => {
    const timeout = setTimeout(() => {
      setFakeLoading(false);
    }, 1000);

    return () => {
      clearTimeout(timeout);
    };
  }, []);

  useEffect(() => {
    scrollRef?.current?.scrollIntoView();
  }, [messages.length]);

  const onInspect = (message: any, index: number) => {
    const query = messages[index - 1].content;
    const history = messages.slice(0, index - 1);
    inspectMessageModal.onOpen(ai, message, history, query);
  };

  return (
    <div className="flex-1 overflow-y-auto px-1 sm:px-4 overflow-x-hidden">
      <InspectMessageModal />
      {ai && (
        <ChatMessage
          isLoading={fakeLoading}
          src={ai.src}
          role="system"
          content={
            ai.introduction || `Hello, I am ${ai.name}, ${ai.description}`
          }
        />
      )}
      {messages.map((message, index) => (
        <ChatMessage
          key={`chat-msg-${index}`}
          src={ai?.src}
          content={message.content}
          role={message.role}
          onInspect={
            canEditAi && message.metadata
              ? () => onInspect(message, index)
              : undefined
          }
        />
      ))}
      {isLoading && <ChatMessage src={ai?.src} role="system" isLoading />}
      {stream && (
        <ChatMessage
          src={ai?.src}
          role="system"
          content={stream}
          scrollRef={scrollRef}
          isAnimated
        />
      )}
      <div ref={scrollRef} />
    </div>
  );
};
