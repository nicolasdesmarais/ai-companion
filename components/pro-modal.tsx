"use client";

import { useEffect, useState } from "react";

import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";
import { useProModal } from "@/hooks/use-pro-modal";
import StripePricingTable from "./stripe-pricing-table";

type Props = {
  orgId: string;
};

export const ProModal = ({ orgId }: Props) => {
  const stripePublishableKey =
    process.env.NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY ?? "";
  const pricingTableId = process.env.NEXT_PUBLIC_STRIPE_PRICING_TABLE_ID ?? "";

  const proModal = useProModal();
  const [isMounted, setIsMounted] = useState(false);

  useEffect(() => {
    setIsMounted(true);
  }, []);

  if (!isMounted) {
    return null;
  }

  return (
    <Dialog open={proModal.isOpen} onOpenChange={proModal.onClose}>
      <DialogContent>
        <DialogHeader className="space-y-4">
          <DialogTitle className="text-center">
            Upgrade AppDirect.ai
          </DialogTitle>
        </DialogHeader>
        <Separator />
        <div className="overflow-auto h-screen">
          <StripePricingTable
            orgId={orgId}
            stripePublishableKey={stripePublishableKey}
            pricingTableId={pricingTableId}
          />
        </div>
      </DialogContent>
    </Dialog>
  );
};
