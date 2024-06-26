"use client";
import { BotAvatar } from "@/components/bot-avatar";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { useChats } from "@/hooks/use-chats";
import { ChatSummaryDto } from "@/src/domain/models/Chats";
import { cn } from "@/src/lib/utils";
import { Loader } from "lucide-react";
import { usePathname, useRouter, useSearchParams } from "next/navigation";
import { useEffect, useRef } from "react";

type Props = {
  className?: string;
};

export const ChatList = ({ className }: Props) => {
  const { chats, fetchChats, loading } = useChats();
  const router = useRouter();
  const pathname = usePathname();
  const searchParams = useSearchParams();
  const itemsRef = useRef<Array<HTMLDivElement | null>>([]);
  const isNew = searchParams.get("new");

  const pinned = chats.filter((chat) => chat.pinPosition);
  const unpinned = chats.filter((chat) => !chat.pinPosition);

  useEffect(() => {
    const index = unpinned.findIndex((chat) => pathname.endsWith(chat.id));
    if (index !== -1) {
      itemsRef.current[index]?.scrollIntoView({
        block: "center",
      });
    }
  }, [pathname, chats]);

  useEffect(() => {
    if (!chats.length || isNew) {
      fetchChats();
    }
  }, [fetchChats, chats.length, isNew]);

  return (
    <div
      className={cn(
        "flex flex-col h-full py-4 px-2 bg-accent/30 overflow-y-auto w-full @container overflow-x-hidden",
        className
      )}
    >
      <div className="flex flex-wrap">
        {pinned.map((chat: ChatSummaryDto) => (
          <div
            className="w-full @4xs:w-1/2 @2xs:w-1/3 @sm:w-1/4 @lg:w-1/5 @4xs:p-1"
            key={chat.id}
          >
            <div
              onClick={() => router.push(`/chat/${chat.id}`)}
              className={cn(
                "rounded-lg p-2 transition",
                pathname.endsWith(chat.id) ? "bg-accent" : "cursor-pointer"
              )}
            >
              <div className="">
                <Avatar className="h-18 w-18">
                  <AvatarImage src={chat.ai.src} crop="w_250,h_250" />
                </Avatar>
              </div>
              <div className="mt-2 text-xs truncate text-center @4xs:text-left">
                {chat.ai.name}
              </div>
            </div>
          </div>
        ))}
      </div>
      {unpinned.map((chat: ChatSummaryDto, index) => (
        <div key={chat.id} className="w-full">
          <div
            ref={(el) => (itemsRef.current[index] = el)}
            onClick={() => router.push(`/chat/${chat.id}`)}
            className={cn(
              "flex @4xs:gap-x-2 items-center @4xs:min-h-20 text-primary rounded-lg p-2 mb-2 transition justify-center w-full",
              pathname.endsWith(chat.id) ? "bg-accent" : "cursor-pointer"
            )}
          >
            <div className="w-full @4xs:w-auto text-center @4xs:text-left">
              <div className="w-full flex flex-col items-center mt-2">
                <BotAvatar src={chat.ai.src} />
              </div>
              <div className="block @4xs:hidden mt-2 text-xs truncate ">
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
                <p className="font-bold text-ellipsis">
                  {chat.ai.name} {chat.name}
                </p>
              </div>
              <div
                className={cn(
                  "text-xs text-muted-foreground w-full text-ellipsis h-8 overflow-hidden "
                )}
              >
                {chat.summary || chat.ai.description}
              </div>
            </div>
          </div>
        </div>
      ))}
      {loading && (
        <div className="flex flex-col items-center justify-center h-full">
          <Loader className="w-12 h-12 spinner" />
        </div>
      )}
    </div>
  );
};
