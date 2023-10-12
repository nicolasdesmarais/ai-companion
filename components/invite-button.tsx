"use client";
import { InviteModal } from "@/components/invite-modal";
import { Button } from "@/components/ui/button";
import { UserPlus2 } from "lucide-react";
import { useState } from "react";

export const InviteButton = () => {
  const [showModal, setShowModal] = useState(false);

  return (
    <>
      <Button
        size="sm"
        variant="premium"
        className="my-2"
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
