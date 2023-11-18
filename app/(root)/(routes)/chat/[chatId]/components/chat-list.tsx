"use client";
import { BotAvatar } from "@/components/bot-avatar";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { useChats } from "@/hooks/use-conversations";
import { cn } from "@/src/lib/utils";
import { usePathname, useRouter } from "next/navigation";
import { useEffect } from "react";

export const ChatList = () => {
  const { chats: conversations, fetchConversations } = useChats();
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
    <div className="hidden md:flex flex-col h-full py-4 px-2 bg-accent/30 overflow-y-auto shrink-0 w-80">
      <div className="flex flex-wrap">
        {pinned.map((conversation: any) => (
          <div className="w-1/3 p-1" key={conversation.id}>
            <div
              onClick={() => router.push(`/chat/${conversation.id}`)}
              className={cn(
                "rounded-lg p-2 transition",
                pathname.endsWith(conversation.id)
                  ? "bg-accent"
                  : "cursor-pointer"
              )}
            >
              <div>
                <Avatar className="h-18 w-18">
                  <AvatarImage src={conversation.ai.src} crop="w_78,h_78" />
                </Avatar>
              </div>
              <div className="mt-2 text-xs truncate">{conversation.name}</div>
            </div>
          </div>
        ))}
      </div>
      {unpinned.map((conversation: any, index) => (
        <div
          onClick={() => router.push(`/chat/${conversation.id}`)}
          className={cn(
            "flex gap-x-2 items-center min-h-20 text-primary rounded-lg p-2 mb-2 transition",
            pathname.endsWith(conversation.id) ? "bg-accent" : "cursor-pointer"
          )}
          key={conversation.id}
        >
          <BotAvatar src={conversation.ai.src} />
          <div
            className={cn(
              "flex flex-col gap-y-1 w-full border-b border-muted-foreground pb-2",
              index === unpinned.length - 1 ||
                pathname.endsWith(conversation.id) ||
                (unpinned[index + 1] &&
                  pathname.endsWith(unpinned[index + 1].id))
                ? "border-none"
                : ""
            )}
          >
            <div className="flex items-center gap-x-2">
              <p className="font-bold text-ellipsis">{conversation.name}</p>
            </div>
            <div
              className={cn(
                "text-xs text-muted-foreground w-full text-ellipsis h-8 overflow-hidden"
              )}
            >
              {conversation.ai.description}
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};
