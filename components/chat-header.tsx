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
import { Companion, Message, Conversation } from "@prisma/client";
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
  conversation: Conversation & {
    messages: Message[];
    companion: Companion;
    _count: {
      messages: number;
    };
  };
}

export const ChatHeader = ({ conversation }: ChatHeaderProps) => {
  const router = useRouter();
  const { user } = useUser();
  const { toast } = useToast();

  const duplicate = async () => {
    const response = await axios.put(
      `/api/v1/conversations/${conversation.id}/duplicate`
    );
    if (response.status === 200) {
      toast({ description: "Conversation duplicated." });
      router.push(`/chat/${response.data.id}`);
    }
  };

  const reset = async () => {
    const response = await axios.put(
      `/api/v1/conversations/${conversation.id}/reset`
    );
    if (response.status === 200) {
      toast({ description: "Conversation reset." });
      router.push(`/chat/${response.data.id}`);
    }
  };

  const pin = async () => {
    const response = await axios.put(
      `/api/v1/conversations/${conversation.id}/pin`
    );
    if (response.status === 200) {
      toast({ description: "Conversation pinned." });
      router.push(`/chat/${response.data.id}`);
    }
  };

  const remove = async () => {
    const response = await axios.delete(
      `/api/v1/conversations/${conversation.id}`
    );
    if (response.status === 200) {
      toast({ description: "Conversation deleted." });
      router.push(`/chat/`);
    }
  };

  return (
    <div className="flex w-full justify-between items-center p-4 bg-accent/30">
      <div className="flex gap-x-2 items-center">
        <BotAvatar src={conversation.companion.src} />
        <div className="flex flex-col gap-y-1">
          <div className="flex items-center gap-x-2">
            <p className="font-bold">{conversation.companion.name}</p>
            <div className="flex items-center text-xs text-muted-foreground">
              <MessagesSquare className="w-3 h-3 mr-1" />
              {conversation._count.messages}
            </div>
          </div>
          <p className="text-xs text-muted-foreground">
            Created by {conversation.companion.userName}
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
          <DropdownMenuItem onClick={() => pin()}>
            <Pin className="w-4 h-4 mr-2" />
            Pin
          </DropdownMenuItem>
          <DropdownMenuItem onClick={() => reset()}>
            <RefreshCw className="w-4 h-4 mr-2" />
            Reset
          </DropdownMenuItem>
          <DropdownMenuItem onClick={() => duplicate()}>
            <CopyPlus className="w-4 h-4 mr-2" />
            Duplicate
          </DropdownMenuItem>
          <DropdownMenuItem onClick={() => remove()}>
            <Trash className="w-4 h-4 mr-2" />
            Remove
          </DropdownMenuItem>
          {user?.id === conversation.companion.userId && (
            <DropdownMenuItem
              onClick={() =>
                router.push(`/companion/${conversation.companion.id}`)
              }
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
