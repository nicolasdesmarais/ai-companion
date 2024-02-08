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

import {
  CreateManageSubscriptionSessionRequest,
  ManageSubscriptionSession,
  OrgSubscriptionDto,
} from "@/src/domain/models/OrgSubscriptions";
import axios from "axios";
import { Loader } from "lucide-react";

type Props = {
  orgId: string;
};

export const ProModal = ({ orgId }: Props) => {
  const stripePublishableKey =
    process.env.NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY ?? "";
  const pricingTableId = process.env.NEXT_PUBLIC_STRIPE_PRICING_TABLE_ID ?? "";

  const proModal = useProModal();
  const [isMounted, setIsMounted] = useState(false);
  const [loading, setLoading] = useState(true);
  const [subscription, setSubscription] = useState<OrgSubscriptionDto>();

  const fetchSubscription = async () => {
    setLoading(true);
    try {
      const response = await axios.get(`/api/v1/org-subscription`);
      setSubscription(response.data);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    setIsMounted(true);
    fetchSubscription();
  }, []);

  const handleUpgrade = async () => {
    try {
      const host = window.location.host;
      const protocol = window.location.protocol;
      const redirectUrl = `${protocol}//${host}/`;
      const input: CreateManageSubscriptionSessionRequest = {
        redirectUrl,
      };

      const response = await axios.post(`/api/v1/org-subscription`, input);
      const data = response.data as ManageSubscriptionSession;
      window.location.href = data.manageSubscriptionRedirectUrl;
    } catch (error) {
      console.error("Failed to upgrade subscription", error);
    }
  };

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
        {loading ? (
          <div className="flex justify-center items-center h-32">
            <Loader className="w-16 h-16 spinner" />
          </div>
        ) : (
          <div className="overflow-auto h-screen">
            {subscription?.externalId ? (
              <>
                <p>
                  You are subscribed to the {subscription.metadata.productName}{" "}
                  plan.
                </p>
                <button
                  className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
                  onClick={handleUpgrade}
                >
                  Upgrade
                </button>
              </>
            ) : (
              <StripePricingTable
                orgId={orgId}
                stripePublishableKey={stripePublishableKey}
                pricingTableId={pricingTableId}
              />
            )}
          </div>
        )}
      </DialogContent>
    </Dialog>
  );
};
