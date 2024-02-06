"use client";
import { useUser } from "@clerk/nextjs";
import React from "react";

interface Props {
  orgId: string;
  stripePublishableKey: string;
  pricingTableId: string;
}

const StripePricingTable = ({
  orgId,
  stripePublishableKey,
  pricingTableId,
}: Props) => {
  const { user } = useUser();

  const pricingTable = React.createElement("stripe-pricing-table", {
    "pricing-table-id": pricingTableId,
    "publishable-key": stripePublishableKey,
    "client-reference-id": orgId,
    "customer-email": user?.primaryEmailAddress?.emailAddress,
    metadata: "AppDirect.ai",
  });
  return (
    <>
      <script async src="https://js.stripe.com/v3/pricing-table.js"></script>
      {pricingTable}
    </>
  );
};
export default StripePricingTable;
