"use client";

import { datadogRum } from "@datadog/browser-rum";

datadogRum.init({
  applicationId: "03817d82-3867-4802-b0d7-b9e8bd374b98",
  clientToken: "pub17023fc825ff17c93288e95d1442a6c1",
  site: "us3.datadoghq.com",
  service: "appdirect.ai",
  env: process.env.NEXT_PUBLIC_VERCEL_ENV || "local",
  version: process.env.NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA || "local",
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
