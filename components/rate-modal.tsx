"use client";

import { Dispatch, SetStateAction, useEffect, useState } from "react";

import { Dialog, DialogContent } from "@/components/ui/dialog";
import { useToast } from "@/components/ui/use-toast";
import { AI } from "@prisma/client";
import { RateAIForm } from "@/components/rate-ai-form";

interface RateModalProps {
  showModal: boolean;
  setShowModal: Dispatch<SetStateAction<boolean>>;
  ai: AI;
}

export const RateModal = ({ showModal, setShowModal, ai }: RateModalProps) => {
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
      description: "Thank you for rating this AI.",
    });
  };

  return (
    <Dialog open={showModal} onOpenChange={() => setShowModal(false)}>
      <DialogContent>
        <RateAIForm ai={ai} onSuccess={onSuccess} />
      </DialogContent>
    </Dialog>
  );
};
