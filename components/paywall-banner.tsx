"use client";

import { useOrgUsage } from "@/hooks/use-org-usage";
import { cn } from "@/src/lib/utils";
import { useClerk } from "@clerk/nextjs";
import { AlertOctagon } from "lucide-react";
import { useEffect } from "react";

interface Props {
  className?: string;
}

export const PaywallBanner = ({ className }: Props) => {
  const clerk = useClerk();
  const { usage, fetchUsage, loading } = useOrgUsage();
  const show = usage.dataUsedInGb > usage.dataUsageLimitInGb;

  useEffect(() => {
    fetchUsage();
  }, [clerk.organization?.id]);

  useEffect(() => {
    fetchUsage();
  }, []);

  if (!show) {
    return null;
  }

  return (
    <div className={cn("w-full z-30", className)}>
      <div className="flex items-center text-center h-8 bg-ring mt-1 pl-4 rounded-md text-sm">
        <AlertOctagon className="w-4 h-4 mr-2" />
        You have reached the limit of your plan. Please ask your administrator
        to upgrade.
      </div>
    </div>
  );
};
