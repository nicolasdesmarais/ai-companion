"use client";

import { Dispatch, SetStateAction, useEffect, useState } from "react";

import { ShareAIForm } from "@/components/share-ai-form";
import { Dialog, DialogContent } from "@/components/ui/dialog";
import { useToast } from "@/components/ui/use-toast";
import { AIDetailDto } from "@/src/domain/models/AI";

interface InviteModalProps {
  showModal: boolean;
  setShowModal: Dispatch<SetStateAction<boolean>>;
  ai: AIDetailDto;
}

export const ShareModal = ({
  showModal,
  setShowModal,
  ai,
}: InviteModalProps) => {
  const [isMounted, setIsMounted] = useState(false);
  const { toast } = useToast();

  useEffect(() => {
    setIsMounted(true);
  }, []);

  if (!isMounted) {
    return null;
  }

  const onSuccess = () => {
    setShowModal(false);
    toast({
      description: "AI shared successfully.",
    });
  };

  return (
    <Dialog open={showModal} onOpenChange={() => setShowModal(false)}>
      <DialogContent>
        <ShareAIForm ai={ai} onSuccess={onSuccess} />
      </DialogContent>
    </Dialog>
  );
};
