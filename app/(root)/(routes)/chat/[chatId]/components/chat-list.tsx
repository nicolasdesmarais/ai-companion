"use client";
import { BotAvatar } from "@/components/bot-avatar";
import { cn } from "@/lib/utils";
import { Companion } from "@prisma/client";
import { usePathname, useRouter } from "next/navigation";

interface ChatListProps {
  companions: Companion[];
}

export const ChatList = ({ companions }: ChatListProps) => {
  const router = useRouter();
  const pathname = usePathname();

  return (
    <div className="flex flex-col h-full p-2 bg-accent/30 space-y-2">
      {companions.map((companion) => (
        <div
          onClick={() => router.push(`/chat/${companion.id}`)}
          className={cn(
            "flex gap-x-2 items-center w-64 h-20 text-primary rounded-lg p-2 transition",
            pathname.endsWith(companion.id)
              ? "bg-accent"
              : "hover:text-primary hover:bg-primary/10 cursor-pointer"
          )}
          key={companion.id}
        >
          <BotAvatar src={companion.src} />
          <div className="flex flex-col gap-y-1">
            <div className="flex items-center gap-x-2">
              <p className="font-bold">{companion.name}</p>
            </div>
            <p className="text-xs text-muted-foreground">
              {companion.description}
            </p>
          </div>
        </div>
      ))}
    </div>
  );
};
