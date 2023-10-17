"use client";
import { Companion, Conversation } from "@prisma/client";
import { useRouter, usePathname } from "next/navigation";
import { BotAvatar } from "@/components/bot-avatar";
import { cn } from "@/lib/utils";
import { useEffect, useState } from "react";

interface ChatListProps {
  initialConversations: (Conversation & {
    companion: Companion;
  })[];
}

export const ChatList = ({ initialConversations }: ChatListProps) => {
  const [conversations, setConversations] = useState<
    (Conversation & {
      companion: Companion;
    })[]
  >(initialConversations);
  const router = useRouter();
  const pathname = usePathname();

  useEffect(() => {
    const fetchConversations = async () => {
      const response = await fetch("/api/v1/conversations");
      if (response.status === 200) {
        const data = await response.json();
        setConversations(data);
      }
    };

    fetchConversations();
  }, []);

  return (
    <div className="hidden sm:flex flex-col h-full p-2 bg-accent/30 space-y-2 overflow-y-auto min-width-24 max-w-md">
      {conversations.map((conversation: any) => (
        <div
          onClick={() => router.push(`/chat/${conversation.id}`)}
          className={cn(
            "flex gap-x-2 items-center h-20 text-primary rounded-lg p-2 transition",
            pathname.endsWith(conversation.id)
              ? "bg-accent"
              : "hover:text-primary hover:bg-primary/10 cursor-pointer"
          )}
          key={conversation.id}
        >
          <BotAvatar src={conversation.companion.src} />
          <div className="flex flex-col gap-y-1">
            <div className="flex items-center gap-x-2">
              <p className="font-bold">{conversation.name}</p>
            </div>
            <p className="text-xs text-muted-foreground">
              {conversation.companion.description}
            </p>
          </div>
        </div>
      ))}
    </div>
  );
};
