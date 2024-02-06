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
  const proModal = useProModal();
  const [isMounted, setIsMounted] = useState(false);
  const [stripePublishableKey, setStripePublishableKey] = useState("");
  const [pricingTableId, setPricingTableId] = useState("");

  useEffect(() => {
    setIsMounted(true);
    setStripePublishableKey(process.env.STRIPE_PUBLISHABLE_KEY || "");
    setPricingTableId(process.env.STRIPE_PRICING_TABLE_ID || "");
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
