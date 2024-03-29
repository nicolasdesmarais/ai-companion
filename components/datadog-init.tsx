"use client";

import { datadogRum } from "@datadog/browser-rum";

datadogRum.init({
  applicationId: process.env.DATADOG_APPLICATION_ID ?? "",
  clientToken: process.env.DATADOG_CLIENT_TOKEN ?? "",
  site: "us3.datadoghq.com",
  service: "appdirect.ai",
  env: process.env.NEXT_PUBLIC_VERCEL_ENV ?? "local",
  version: process.env.NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA ?? "local",
  sessionSampleRate: 100,
  sessionReplaySampleRate: 20,
  trackUserInteractions: true,
  trackResources: true,
  trackLongTasks: true,
  defaultPrivacyLevel: "mask-user-input",
});

interface Props {
  userId: string;
}

export default function DatadogInit({ userId }: Props) {
  if (userId) {
    datadogRum.setUser({ id: userId });
  }
  return null;
}
