"use client";

import { useEffect, useState } from "react";

import { RateAIForm } from "@/components/rate-ai-form";
import { Dialog, DialogContent } from "@/components/ui/dialog";
import { useToast } from "@/components/ui/use-toast";
import { useRateAI } from "@/hooks/use-rate-ai";
import { AIDetailDto } from "@/src/domain/ports/api/AIApi";

interface RateModalProps {
  ai: AIDetailDto;
}

export const RateModal = ({ ai }: RateModalProps) => {
  const [isMounted, setIsMounted] = useState(false);
  const { toast } = useToast();
  const { isOpen, onClose } = useRateAI();

  useEffect(() => {
    setIsMounted(true);
  }, []);

  if (!isMounted) {
    return null;
  }

  const onSuccess = () => {
    onClose();
    toast({
      description: "Thank you for rating this AI.",
    });
  };

  return (
    <Dialog open={isOpen} onOpenChange={() => onClose()}>
      <DialogContent>
        <RateAIForm ai={ai} onSuccess={onSuccess} />
      </DialogContent>
    </Dialog>
  );
};
