import { BotAvatar } from "@/components/bot-avatar";
import { AIDetailDto } from "@/src/domain/models/AI";
import { ChatSummaryDto } from "@/src/domain/models/Chats";
import { cn } from "@/src/lib/utils";
import Link from "next/link";

type Props = {
  className?: string;
  ais: AIDetailDto[];
  ai: AIDetailDto | null;
};

export const PublicChatList = async ({ className, ais, ai }: Props) => {
  const now = new Date();

  const chats = ais.map((ai) => {
    return {
      id: ai.id,
      createdAt: now,
      updatedAt: now,
      messagedAt: now,
      name: ai.name,
      summary: ai.description,
      orgId: "",
      userId: "",
      pinPosition: null,
      ai,
    } as ChatSummaryDto;
  });

  return (
    <div
      className={cn(
        "flex flex-col h-full py-4 px-2 bg-accent/30 overflow-y-auto w-full @container",
        className
      )}
    >
      {chats.map((chat: ChatSummaryDto, index) => (
        <div key={`chat-${index}`} className="w-full">
          <Link href={`/public/ai/${chat.ai.id}`}>
            <div
              className={cn(
                "flex @4xs:gap-x-2 items-center @4xs:min-h-20 text-primary rounded-lg p-2 mb-2 transition justify-center w-full",
                ai?.id === chat.id ? "bg-accent" : "cursor-pointer"
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
                  ai?.id === chat.id ? "border-none" : ""
                )}
              >
                <div className="flex items-center gap-x-2">
                  <p className="font-bold text-ellipsis">{chat.ai.name}</p>
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
          </Link>
        </div>
      ))}
    </div>
  );
};
