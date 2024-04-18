"use client";
import { InviteModal } from "@/components/invite-modal";
import { Button } from "@/components/ui/button";
import { cn } from "@/src/lib/utils";
import { UserPlus2 } from "lucide-react";
import { useState } from "react";

interface Props {
  className?: string;
}

export const InviteButton = ({ className }: Props) => {
  const [showModal, setShowModal] = useState(false);

  return (
    <>
      <Button
        size="sm"
        variant="ring"
        className={cn("my-2", className)}
        type="button"
        onClick={() => setShowModal(true)}
      >
        Invite
        <UserPlus2 className="h-4 w-4 fill-white text-white ml-2" />
      </Button>
      <InviteModal showModal={showModal} setShowModal={setShowModal} />
    </>
  );
};
