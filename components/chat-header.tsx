"use client";

import axios from "axios";
import {
  BadgeCheck,
  Edit,
  ExternalLink,
  MessageSquarePlus,
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
import { PaywallBanner } from "@/components/paywall-banner";
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
import { useConfirmModal } from "@/hooks/use-confirm-modal";
import { useRateAI } from "@/hooks/use-rate-ai";
import { AIDetailDto } from "@/src/domain/models/AI";
import { ChatDetailDto } from "@/src/domain/models/Chats";
import { AIVisibility } from "@prisma/client";
import { useState } from "react";
import { RateModal } from "./rate-modal";
import { ShareModal } from "./share-modal";
import { StarRating } from "./star-rating";

interface ChatHeaderProps {
  ai: AIDetailDto | null;
  chat: ChatDetailDto;
  canEditAi: boolean;
  canApproveAi: boolean;
}

export const ChatHeader = ({
  ai,
  chat,
  canEditAi,
  canApproveAi,
}: ChatHeaderProps) => {
  const router = useRouter();
  const { toast } = useToast();
  const { chats, fetchChats } = useChats();
  const aiProfile = useAIProfile();
  const [showShareModal, setShowShareModal] = useState(false);
  const rateAI = useRateAI();
  const confirmModal = useConfirmModal();
  const [isApproved, setIsApproved] = useState(ai?.isApprovedByOrg);

  const duplicate = async () => {
    const response = await axios.put(`/api/v1/chats/${chat.id}/duplicate`);
    if (response.status === 200) {
      toast({ description: "Chat duplicated." });
      router.push(`/chat/${response.data.id}?new=true`);
      fetchChats();
    }
  };

  const reset = async () => {
    confirmModal.onOpen(
      <div className="flex items-center">Reset Chat Thread?</div>,
      <div>
        You can continue chatting with {ai?.name} but this thread&apos;s chat
        history will be deleted.
      </div>,
      () => {},
      <div className="flex flex-row-reverse w-full">
        <Button
          variant="destructive"
          onClick={async () => {
            confirmModal.onLoading(true);
            const response = await axios.put(`/api/v1/chats/${chat.id}/reset`);
            if (response.status === 200) {
              toast({ description: "Chat reset." });
              router.push(`/chat/${response.data.id}?new=true`);
              confirmModal.onClose();
            }
          }}
          type="button"
        >
          Reset
        </Button>
      </div>
    );
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
    confirmModal.onOpen(
      <div className="flex items-center">Delete Chat Thread?</div>,
      <div>
        You can start a new chat with {ai?.name} at any time but this
        thread&apos;s chat history will be lost.
      </div>,
      () => {},
      <div className="flex flex-row-reverse w-full">
        <Button
          variant="destructive"
          onClick={async () => {
            confirmModal.onLoading(true);
            const response = await axios.delete(`/api/v1/chats/${chat.id}`);
            if (response.status === 204) {
              toast({ description: "Chat deleted." });
              router.push(`/chat/`);
              fetchChats();
              confirmModal.onClose();
            }
          }}
          type="button"
        >
          Delete
        </Button>
      </div>
    );
  };

  const approve = async () => {
    confirmModal.onOpen(
      <div className="flex items-center">
        Mark as Company Approved
        <BadgeCheck className="w-6 h-6 ml-2 text-ring" />
      </div>,
      <div>
        By marking {ai?.name} as Company Approved it will receive a blue
        checkmark. This will make it clear to your employees that this AI has
        been identified by your company as high quality. You can revoke this
        status at any time.
      </div>,
      () => {},
      <div className="flex flex-row-reverse w-full">
        <Button
          variant="ring"
          onClick={async () => {
            confirmModal.onClose();
            const response = await axios.put(`/api/v1/ai/${ai?.id}/approve`);
            if (response.status === 200) {
              toast({ description: "AI Approved." });
              setIsApproved(true);
              fetchChats();
            }
          }}
          type="button"
        >
          Mark as Approved
        </Button>
      </div>
    );
  };

  const revoke = async () => {
    confirmModal.onOpen(
      <div className="flex items-center">
        Revoke Company Approved Status
        <BadgeCheck className="w-6 h-6 ml-2 text-ring" />
      </div>,
      <div>
        By revoking the Company Approved status for {ai?.name} it will no longer
        display a blue checkmark or show up in the Company Approved filter. You
        can add the Company Approved status back at any time.
      </div>,
      () => {},
      <div className="flex flex-row-reverse w-full">
        <Button
          variant="ring"
          onClick={async () => {
            confirmModal.onClose();
            const response = await axios.put(`/api/v1/ai/${ai?.id}/revoke`);
            if (response.status === 200) {
              toast({ description: "AI Approval Revoked." });
              setIsApproved(false);
              fetchChats();
            }
          }}
          type="button"
        >
          Revoke Approved Status
        </Button>
      </div>
    );
  };

  const foundChat = chats.find((c) => c.id === chat.id);
  const thisChat = foundChat !== undefined ? foundChat : chat;
  return (
    <div className="flex flex-col p-4 pb-3 bg-accent/30">
      <div className="flex w-full justify-between items-center pl-12 md:pl-0">
        <div className="flex gap-x-2 items-center">
          <BotAvatar src={chat.ai.src} />
          <div className="flex flex-col gap-y-1">
            <div className="flex">
              <p className="font-bold">{chat.ai.name}</p>
              {isApproved ? (
                <BadgeCheck className="w-6 h-6 ml-1 text-ring hidden md:block" />
              ) : null}
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
          {ai &&
            (canEditAi ||
              ai.visibility === AIVisibility.ANYONE_WITH_LINK ||
              ai.visibility === AIVisibility.ORGANIZATION) && (
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
              {thisChat.pinPosition ? (
                <DropdownMenuItem onClick={() => unpin()}>
                  <PinOff className="w-4 h-4 mr-2" />
                  Unpin
                </DropdownMenuItem>
              ) : (
                ai && (
                  <DropdownMenuItem onClick={() => pin()}>
                    <Pin className="w-4 h-4 mr-2" />
                    Pin
                  </DropdownMenuItem>
                )
              )}
              {ai && (
                <DropdownMenuItem onClick={() => reset()}>
                  <RefreshCw className="w-4 h-4 mr-2" />
                  Reset
                </DropdownMenuItem>
              )}
              {ai && (
                <DropdownMenuItem onClick={() => duplicate()}>
                  <MessageSquarePlus className="w-4 h-4 mr-2" />
                  New Chat
                </DropdownMenuItem>
              )}
              <DropdownMenuItem onClick={() => remove()}>
                <Trash className="w-4 h-4 mr-2" />
                Remove
              </DropdownMenuItem>
              {ai && (
                <DropdownMenuItem onClick={() => rateAI.onOpen()}>
                  <Star className="w-4 h-4 mr-2" />
                  Rate
                </DropdownMenuItem>
              )}
              {ai && (canApproveAi || canEditAi) && (
                <div className="text-xxs uppercase border-b dar:text-white/30 m-1">
                  Admin
                </div>
              )}
              {ai && canApproveAi && !isApproved && (
                <DropdownMenuItem onClick={() => approve()}>
                  <BadgeCheck className="w-4 h-4 mr-2" />
                  Approve
                </DropdownMenuItem>
              )}
              {ai && canApproveAi && isApproved && (
                <DropdownMenuItem onClick={() => revoke()}>
                  <BadgeCheck className="w-4 h-4 mr-2" />
                  Revoke
                </DropdownMenuItem>
              )}
              {ai && canEditAi && (
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
      {ai && <RateModal ai={ai} />}
      <PaywallBanner />
    </div>
  );
};
