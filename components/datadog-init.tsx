"use client";

import { datadogRum } from "@datadog/browser-rum";

datadogRum.init({
  applicationId: "c4cdbfab-7db8-452b-a7ba-37c251189ee2",
  clientToken: "pub3fb1d50e7a6f5444ff86c55271220170",
  site: "datadoghq.com",
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

export default function DatadogInit() {
  return null;
}
