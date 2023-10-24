"use client";
import { BotAvatar } from "@/components/bot-avatar";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { useConversations } from "@/hooks/use-conversations";
import { cn } from "@/src/lib/utils";
import { usePathname, useRouter } from "next/navigation";
import { useEffect } from "react";

export const ChatList = () => {
  const { conversations, fetchConversations } = useConversations();
  const router = useRouter();
  const pathname = usePathname();

  useEffect(() => {
    fetchConversations();
  }, []);

  const pinned = conversations.filter(
    (conversation) => conversation.pinPosition
  );

  const unpinned = conversations.filter(
    (conversation) => !conversation.pinPosition
  );

  return (
    <div className="hidden sm:flex flex-col h-full p-2 bg-accent/30 overflow-y-auto w-96">
      <div className="flex flex-wrap">
        {pinned.map((conversation: any) => (
          <div className="w-1/3 p-1" key={conversation.id}>
            <div
              onClick={() => router.push(`/chat/${conversation.id}`)}
              className={cn(
                "rounded-lg p-2 transition",
                pathname.endsWith(conversation.id)
                  ? "bg-accent"
                  : "hover:text-primary hover:bg-primary/10 cursor-pointer"
              )}
            >
              <div>
                <Avatar className="h-18 w-18">
                  <AvatarImage src={conversation.companion.src} />
                </Avatar>
              </div>
              <div className="mt-2 text-xs truncate">{conversation.name}</div>
            </div>
          </div>
        ))}
      </div>
      {unpinned.map((conversation: any) => (
        <div
          onClick={() => router.push(`/chat/${conversation.id}`)}
          className={cn(
            "flex gap-x-2 items-center h-20 text-primary rounded-lg p-2 mb-2 transition",
            pathname.endsWith(conversation.id)
              ? "bg-accent"
              : "hover:text-primary hover:bg-primary/10 cursor-pointer"
          )}
          key={conversation.id}
        >
          <BotAvatar src={conversation.companion.src} />
          <div className="flex flex-col gap-y-1 w-full">
            <div className="flex items-center gap-x-2">
              <p className="font-bold text-ellipsis">{conversation.name}</p>
            </div>
            <div className="text-xs text-muted-foreground w-full pb-2 border-b border-muted-foreground text-ellipsis">
              {conversation.companion.description}
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};
