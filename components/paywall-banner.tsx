"use client";

import { useOrgUsage } from "@/hooks/use-org-usage";
import { useProModal } from "@/hooks/use-pro-modal";
import { cn } from "@/src/lib/utils";
import { useClerk } from "@clerk/nextjs";
import { AlertOctagon } from "lucide-react";
import { useEffect } from "react";

interface Props {
  className?: string;
}

export const PaywallBanner = ({ className }: Props) => {
  const proModal = useProModal();
  const clerk = useClerk();
  const { usage, fetchUsage } = useOrgUsage();
  const show = usage.dataUsedInGb > usage.dataUsageLimitInGb;

  const orgMembership = clerk.user?.organizationMemberships.find(
    (membership) => membership.organization.id === clerk.organization?.id
  );

  const isAdmin = orgMembership?.role === "admin";

  useEffect(() => {
    fetchUsage();
  }, [clerk.organization?.id]);

  useEffect(() => {
    fetchUsage();
  }, []);

  if (!show) {
    return null;
  }

  console.log("clerk", orgMembership);
  return (
    <div className={cn("w-full z-30", className)}>
      <div className="flex items-center text-center h-8 bg-ring mt-1 pl-4 rounded-md text-sm">
        <AlertOctagon className="w-4 h-4 mr-2" />
        {isAdmin ? (
          <span>
            You have reached the limit of your plan.{" "}
            <span
              className="underline cursor-pointer"
              onClick={proModal.onOpen}
            >
              Please upgrade
            </span>
          </span>
        ) : (
          <span>
            You have reached the limit of your plan. Please ask your
            administrator to upgrade.
          </span>
        )}
      </div>
    </div>
  );
};
