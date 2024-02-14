"use client";

import { cn } from "@/src/lib/utils";
import axios from "axios";
import { AlertOctagon } from "lucide-react";
import { useEffect, useState } from "react";

interface Props {
  className?: string;
}

export const PaywallBanner = ({ className }: Props) => {
  const [show, setShow] = useState(false);

  useEffect(() => {
    const fetchUsage = async () => {
      const result = await axios.get(`/api/v1/usage/org`);
      if (result.data.dataUsageLimitInGb) {
        setShow(result.data.dataUsedInGb > result.data.dataUsageLimitInGb);
      }
    };
    fetchUsage();
  });

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
