"use client";

import { useUser } from "@clerk/nextjs";
import axios from "axios";
import {
  CopyPlus,
  Edit,
  ExternalLink,
  MessagesSquare,
  MoreVertical,
  Pin,
  PinOff,
  RefreshCw,
  Star,
  Trash,
} from "lucide-react";
import { useRouter } from "next/navigation";

import { BotAvatar } from "@/components/bot-avatar";
import { Button } from "@/components/ui/button";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { useToast } from "@/components/ui/use-toast";
import { useAIProfile } from "@/hooks/use-ai-profile";
import { useChats } from "@/hooks/use-chats";
import { useRateAI } from "@/hooks/use-rate-ai";
import { AIDetailDto } from "@/src/domain/ports/api/AIApi";
import { ChatDetailDto } from "@/src/domain/ports/api/ChatsApi";
import { useState } from "react";
import { RateModal } from "./rate-modal";
import { ShareModal } from "./share-modal";
import { StarRating } from "./star-rating";

interface ChatHeaderProps {
  ai: AIDetailDto;
  chat: ChatDetailDto;
  canEditAi: boolean;
}

export const ChatHeader = ({ ai, chat, canEditAi }: ChatHeaderProps) => {
  const router = useRouter();
  const { user } = useUser();
  const { toast } = useToast();
  const { chats, fetchChats } = useChats();
  const aiProfile = useAIProfile();
  const [showShareModal, setShowShareModal] = useState(false);
  const rateAI = useRateAI();
  console.log(ai);
  const duplicate = async () => {
    const response = await axios.put(`/api/v1/chats/${chat.id}/duplicate`);
    if (response.status === 200) {
      toast({ description: "Chat duplicated." });
      router.push(`/chat/${response.data.id}`);
    }
  };

  const reset = async () => {
    const response = await axios.put(`/api/v1/chats/${chat.id}/reset`);
    if (response.status === 200) {
      toast({ description: "Chat reset." });
      router.push(`/chat/${response.data.id}`);
    }
  };

  const pin = async () => {
    const pinned = chats.filter((chat) => chat.pinPosition);
    if (pinned.length >= 9) {
      toast({
        variant: "destructive",
        description:
          "You can only pin up to 9 chats. To pin this chat, unpin another one first.",
      });
      return;
    }
    const response = await axios.put(`/api/v1/chats/${chat.id}/pin`);
    if (response.status === 200) {
      toast({ description: "Chat pinned." });
      fetchChats();
    }
  };

  const unpin = async () => {
    const response = await axios.put(`/api/v1/chats/${chat.id}/unpin`);
    if (response.status === 200) {
      toast({ description: "Chat unpinned." });
      fetchChats();
    }
  };

  const remove = async () => {
    const response = await axios.delete(`/api/v1/chats/${chat.id}`);
    if (response.status === 204) {
      toast({ description: "Chat deleted." });
      router.push(`/chat/`);
    }
  };

  return (
    <div className="flex flex-col p-4 pb-3 bg-accent/30">
      <div className="flex w-full justify-between items-center">
        <div className="flex gap-x-2 items-center">
          <BotAvatar src={ai.src} />
          <div className="flex flex-col gap-y-1">
            <p className="font-bold">{ai.name}</p>
            <div className="flex items-center gap-x-2">
              <p className="text-xs text-muted-foreground">
                Created by {ai.userName}
              </p>
              <div className="flex items-center text-xs text-muted-foreground">
                <MessagesSquare className="w-3 h-3 mr-1" />
                {ai.messageCount}
              </div>
            </div>
          </div>
        </div>
        <div className="flex">
          {(user?.id === ai.userId || ai.visibility === "PUBLIC") && (
            <Button
              variant="ghost"
              size="icon"
              className="mr-4"
              type="button"
              onClick={() => setShowShareModal(true)}
            >
              <ExternalLink />
            </Button>
          )}
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" size="icon">
                <MoreVertical />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              {chat.pinPosition ? (
                <DropdownMenuItem onClick={() => unpin()}>
                  <PinOff className="w-4 h-4 mr-2" />
                  Unpin
                </DropdownMenuItem>
              ) : (
                <DropdownMenuItem onClick={() => pin()}>
                  <Pin className="w-4 h-4 mr-2" />
                  Pin
                </DropdownMenuItem>
              )}
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
              <DropdownMenuItem onClick={() => rateAI.onOpen()}>
                <Star className="w-4 h-4 mr-2" />
                Rate
              </DropdownMenuItem>
              {canEditAi && (
                <DropdownMenuItem
                  onClick={() => router.push(`/ai/${ai.id}/edit`)}
                >
                  <Edit className="w-4 h-4 mr-2" />
                  Edit AI
                </DropdownMenuItem>
              )}
            </DropdownMenuContent>
          </DropdownMenu>
        </div>
      </div>
      <div className="flex ml-14">
        <StarRating
          value={Math.round(ai.rating)}
          count={ai.ratingCount}
          hideCount={true}
          onClick={() => router.push("#user-ratings")}
        />
        <div className="text-xs text-muted-foreground">
          <Button
            variant="link"
            size="xs"
            type="button"
            onClick={() => router.push("#user-ratings")}
          >
            {ai.ratingCount} {ai.ratingCount === 1 ? "Rating" : "Ratings"}
          </Button>
          |
          <Button
            variant="link"
            size="xs"
            type="button"
            onClick={() => aiProfile.onOpen()}
          >
            View Profile
          </Button>
        </div>
      </div>
      <ShareModal
        showModal={showShareModal}
        setShowModal={setShowShareModal}
        ai={ai}
      />
      <RateModal ai={ai} />
    </div>
  );
};
