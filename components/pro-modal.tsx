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
import { OrgSubscriptionStatus } from "@prisma/client";
import axios from "axios";
import { Loader } from "lucide-react";
import Link from "next/link";
import { Button } from "./ui/button";

type Props = {
  orgId: string;
};

export const ProModal = ({ orgId }: Props) => {
  const stripePublishableKey =
    process.env.NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY ?? "";
  const pricingTableId = process.env.NEXT_PUBLIC_STRIPE_PRICING_TABLE_ID ?? "";

  const proModal = useProModal();
  const [isMounted, setIsMounted] = useState(false);
  const [isLoading, setLoading] = useState(true);
  const [isManageSessionLoading, setManageSessionLoading] = useState(false);
  const [subscription, setSubscription] = useState<OrgSubscriptionDto>();

  const hasActiveSubscription = (subscription?: OrgSubscriptionDto) => {
    return (
      subscription?.externalSubscriptionId &&
      subscription?.status === OrgSubscriptionStatus.ACTIVE
    );
  };

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
    setManageSessionLoading(true);
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
    } finally {
      setManageSessionLoading(false);
    }
  };

  if (!isMounted) {
    return null;
  }

  return (
    <Dialog open={proModal.isOpen} onOpenChange={proModal.onClose}>
      <DialogContent className="bg-[#222020] text-white">
        <DialogHeader className="space-y-4">
          <DialogTitle className="text-center">
            Upgrade AppDirect.ai
          </DialogTitle>
        </DialogHeader>
        <Separator />
        {isLoading ? (
          <div className="flex justify-center items-center h-32">
            <Loader className="w-16 h-16 spinner" />
          </div>
        ) : (
          <div className="overflow-auto h-screen">
            {hasActiveSubscription(subscription) ? (
              <>
                <p>
                  You are subscribed to the {subscription?.metadata.productName}{" "}
                  plan.
                </p>
                <Button
                  type="button"
                  variant="outline"
                  onClick={handleUpgrade}
                  disabled={isManageSessionLoading}
                >
                  Update Plan
                  {isManageSessionLoading && (
                    <Loader className="w-4 h-4 ml-2 spinner" />
                  )}
                </Button>
              </>
            ) : (
              <>
                <div className="flex flex-col items-center">
                  <div className="">
                    <h2 className="text-lg font-bold mt-2">Free Tier</h2>
                    <div className="mb-2 text-muted-foreground text-sm">
                      200MB of Data Usage
                    </div>
                    <div className="text-2xl font-bold">FREE</div>
                  </div>
                </div>
                <StripePricingTable
                  orgId={orgId}
                  stripePublishableKey={stripePublishableKey}
                  pricingTableId={pricingTableId}
                />
                <div className="flex flex-col items-center mb-32">
                  <div className="">
                    <h2 className="text-lg font-bold mb-8">Enterprise</h2>
                    <Link
                      href="/contact"
                      className="bg-[#4d70a8] text-white rounded-full py-3 px-8 text-muted-foreground"
                    >
                      Contact us
                    </Link>
                  </div>
                </div>
              </>
            )}
          </div>
        )}
      </DialogContent>
    </Dialog>
  );
};
