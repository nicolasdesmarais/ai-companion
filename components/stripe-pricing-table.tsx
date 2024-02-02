"use client";
import React from "react";

const StripePricingTable = () => {
  const pricingTable = React.createElement("stripe-pricing-table", {
    "pricing-table-id": "prctbl_1Of4OLA6zqpxc81g5sAoiJKq",
    "publishable-key":
      "pk_test_51JLZfwA6zqpxc81gSMxDSWEFUB8RJUzJlPAJrBtXTVPFSEE28aPmVJpEVAXMWiCFL4NyuDGL2Q2nC3gu2Shv2Ab400GVId3lPY",
  });
  return (
    <>
      <script async src="https://js.stripe.com/v3/pricing-table.js"></script>
      {pricingTable}
    </>
  );
};
export default StripePricingTable;
