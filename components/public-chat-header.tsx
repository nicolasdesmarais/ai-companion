"use client";

import {
  CopyPlus,
  ExternalLink,
  MessagesSquare,
  MoreVertical,
  Pin,
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
import { useAIProfile } from "@/hooks/use-ai-profile";
import { AIDetailDto } from "@/src/domain/models/AI";
import { ChatDetailDto } from "@/src/domain/models/Chats";
import Link from "next/link";
import { useState } from "react";
import { ShareModal } from "./share-modal";
import { StarRating } from "./star-rating";

interface Props {
  ai: AIDetailDto | null;
  chat: ChatDetailDto;
}

export const PublicChatHeader = ({ ai, chat }: Props) => {
  const router = useRouter();
  const aiProfile = useAIProfile();
  const [showShareModal, setShowShareModal] = useState(false);

  return (
    <div className="flex flex-col p-4 pb-3 bg-accent/30">
      <div className="flex w-full justify-between items-center pl-12 md:pl-0">
        <div className="flex gap-x-2 items-center">
          <BotAvatar src={chat.ai.src} />
          <div className="flex flex-col gap-y-1">
            <div className="flex">
              <p className="font-bold">{chat.ai.name}</p>
            </div>
            <div className="items-center gap-x-2 hidden md:flex">
              <p className="text-xs text-muted-foreground">
                Created by {chat.ai.userName}
              </p>
              {ai && (
                <div className="flex items-center text-xs text-muted-foreground">
                  <MessagesSquare className="w-3 h-3 mr-1" />
                  {ai.messageCount}
                </div>
              )}
            </div>
          </div>
        </div>

        <div className="flex">
          {ai && (
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
              <Link href="/sign-up">
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

                <DropdownMenuItem>
                  <Star className="w-4 h-4 mr-2" />
                  Rate
                </DropdownMenuItem>
              </Link>
            </DropdownMenuContent>
          </DropdownMenu>
        </div>
      </div>

      {ai && (
        <div className="flex ml-10 md:ml-14 mt-2 md:mt-0">
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
      )}
      {ai && (
        <ShareModal
          showModal={showShareModal}
          setShowModal={setShowShareModal}
          ai={ai}
        />
      )}
    </div>
  );
};
