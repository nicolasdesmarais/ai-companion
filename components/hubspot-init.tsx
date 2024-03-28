"use client";

import Script from "next/script";

const isProd = process.env.NEXT_PUBLIC_VERCEL_ENV === "production";

interface Props {}

export default function HubSpotInit({}: Props) {
  if (isProd) {
    return (
      <Script
        id="hs-script-loader"
        strategy="afterInteractive"
        src="//js.hs-scripts.com/43634300.js"
      />
    );
  }

  return null;
}
