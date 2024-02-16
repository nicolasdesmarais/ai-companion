import { BotAvatar } from "@/components/bot-avatar";
import { ChatSummaryDto } from "@/src/domain/models/Chats";
import { cn } from "@/src/lib/utils";

type Props = {
  className?: string;
};

export const PublicChatList = ({ className }: Props) => {
  const chats = [] as ChatSummaryDto[];
  const activeChat = "" as string;

  return (
    <div
      className={cn(
        "flex flex-col h-full py-4 px-2 bg-accent/30 overflow-y-auto w-full @container",
        className
      )}
    >
      {chats.map((chat: ChatSummaryDto, index) => (
        <div key={`chat-${index}`} className="w-full">
          <div
            className={cn(
              "flex @4xs:gap-x-2 items-center @4xs:min-h-20 text-primary rounded-lg p-2 mb-2 transition justify-center w-full",
              activeChat === chat.id ? "bg-accent" : "cursor-pointer"
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
                activeChat === chat.id ? "border-none" : ""
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
        </div>
      ))}
    </div>
  );
};
