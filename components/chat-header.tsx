"use client";

import axios from "axios";
import {
  Edit,
  MessagesSquare,
  MoreVertical,
  Trash,
  Pin,
  RefreshCw,
  CopyPlus,
} from "lucide-react";
import { useRouter } from "next/navigation";
import { Companion, Message } from "@prisma/client";
import { useUser } from "@clerk/nextjs";

import { Button } from "@/components/ui/button";
import { BotAvatar } from "@/components/bot-avatar";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { useToast } from "@/components/ui/use-toast";

interface ChatHeaderProps {
  companion: Companion;
  messageCount: number;
}

export const ChatHeader = ({ companion, messageCount }: ChatHeaderProps) => {
  const router = useRouter();
  const { user } = useUser();
  const { toast } = useToast();

  return (
    <div className="flex w-full justify-between items-center p-4 bg-accent/30">
      <div className="flex gap-x-2 items-center">
        <BotAvatar src={companion.src} />
        <div className="flex flex-col gap-y-1">
          <div className="flex items-center gap-x-2">
            <p className="font-bold">{companion.name}</p>
            <div className="flex items-center text-xs text-muted-foreground">
              <MessagesSquare className="w-3 h-3 mr-1" />
              {messageCount}
            </div>
          </div>
          <p className="text-xs text-muted-foreground">
            Created by {companion.userName}
          </p>
        </div>
      </div>

      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <Button variant="secondary" size="icon">
            <MoreVertical />
          </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="end">
          <DropdownMenuItem>
            <Pin className="w-4 h-4 mr-2" />
            Pin
          </DropdownMenuItem>
          <DropdownMenuItem>
            <RefreshCw className="w-4 h-4 mr-2" />
            Reset
          </DropdownMenuItem>
          <DropdownMenuItem>
            <CopyPlus className="w-4 h-4 mr-2" />
            Duplicate
          </DropdownMenuItem>
          <DropdownMenuItem>
            <Trash className="w-4 h-4 mr-2" />
            Remove
          </DropdownMenuItem>
          {user?.id === companion.userId && (
            <DropdownMenuItem
              onClick={() => router.push(`/companion/${companion.id}`)}
            >
              <Edit className="w-4 h-4 mr-2" />
              Edit AI
            </DropdownMenuItem>
          )}
        </DropdownMenuContent>
      </DropdownMenu>
    </div>
  );
};
