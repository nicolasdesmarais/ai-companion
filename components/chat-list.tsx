"use client";
import { BotAvatar } from "@/components/bot-avatar";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { useChats } from "@/hooks/use-chats";
import { ChatSummaryDto } from "@/src/domain/ports/api/ChatsApi";
import { cn } from "@/src/lib/utils";
import { usePathname, useRouter } from "next/navigation";
import { useEffect } from "react";

type Props = {
  className?: string;
  isMobile?: boolean;
};

export const ChatList = ({ className, isMobile = false }: Props) => {
  const { chats, fetchChats } = useChats();
  const router = useRouter();
  const pathname = usePathname();

  useEffect(() => {
    fetchChats();
  }, []);

  const pinned = chats.filter((chat) => chat.pinPosition);

  const unpinned = chats.filter((chat) => !chat.pinPosition);

  return (
    <div
      className={cn(
        "flex flex-col h-full py-4 px-2 bg-accent/30 overflow-y-auto w-full h-full @container",
        className
      )}
    >
      <div className="flex flex-wrap">
        {pinned.map((chat: ChatSummaryDto) => (
          <div
            className="w-full @4xs:w-1/2 @2xs:w-1/3 @sm:w-1/4 @lg:w-1/5 p-1"
            key={chat.id}
          >
            <div
              onClick={() => router.push(`/chat/${chat.id}`)}
              className={cn(
                "rounded-lg p-2 transition",
                pathname.endsWith(chat.id) ? "bg-accent" : "cursor-pointer"
              )}
            >
              <div>
                <Avatar className="h-18 w-18">
                  <AvatarImage src={chat.ai.src} crop="w_78,h_78" />
                </Avatar>
              </div>
              <div className="mt-2 text-xs truncate text-center @4xs:text-left">
                {chat.name}
              </div>
            </div>
          </div>
        ))}
      </div>
      {unpinned.map((chat: ChatSummaryDto, index) => (
        <div
          onClick={() => router.push(`/chat/${chat.id}`)}
          className={cn(
            "flex gap-x-2 items-center min-h-20 text-primary rounded-lg p-2 mb-2 transition justify-center",
            pathname.endsWith(chat.id) ? "bg-accent" : "cursor-pointer"
          )}
          key={chat.id}
        >
          <div className="flex flex-col items-center">
            <BotAvatar src={chat.ai.src} />
            <div className="block @4xs:hidden mt-2 text-xs truncate text-center @4xs:text-left">
              {chat.name}
            </div>
          </div>
          <div
            className={cn(
              "hidden @4xs:flex flex-col gap-y-1 w-full border-b border-muted-foreground pb-2",
              index === unpinned.length - 1 ||
                pathname.endsWith(chat.id) ||
                (unpinned[index + 1] &&
                  pathname.endsWith(unpinned[index + 1].id))
                ? "border-none"
                : ""
            )}
          >
            <div className="flex items-center gap-x-2">
              <p className="font-bold text-ellipsis">{chat.name}</p>
            </div>
            <div
              className={cn(
                "text-xs text-muted-foreground w-full text-ellipsis h-8 overflow-hidden "
              )}
            >
              {chat.ai.description}
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};
