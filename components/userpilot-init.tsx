"use client";

import Script from "next/script";

const isProd = process.env.NEXT_PUBLIC_VERCEL_ENV === "production";

interface Props {
  userId: string;
  orgId: string;
}

if (window) {
  (window as any).userpilotSettings = { token: "NX-4e374669" };
}

export default function UserPilotInit({ userId, orgId }: Props) {
  const onReady = () => {
    if (userId) {
      (window as any).userpilot.identify(userId, {
        company: {
          id: orgId,
        },
      });
    } else {
      (window as any).userpilot.anonymous();
    }
  };

  if (isProd) {
    return (
      <Script
        id="userpilot-sdk"
        strategy="afterInteractive"
        src="https://js.userpilot.io/sdk/latest.js"
        onReady={onReady}
      />
    );
  }

  return null;
}
