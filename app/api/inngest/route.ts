import { clerkWebhookReceived } from "@/src/adapter-in/inngest/clerk-workflows";
import {
  dataSourceDeleteRequested,
  dataSourceMigrationRequested,
  deleteUnusedKnowledges,
  loadKnowledgeChunk,
  onDataSourceInitialized,
  onDataSourceItemListReceived,
  onDataSourceRefreshRequested,
  onKnowledgeContentRetrieved,
  onKnowledgeInitialized,
  refreshDataSources,
} from "@/src/adapter-in/inngest/datasource-workflows";
import { googleDriveFolderScanInitiated } from "@/src/adapter-in/inngest/google-drive-workflows";
import { onedriveFolderScanInitiated } from "@/src/adapter-in/inngest/onedrive-workflows";
import { stripeWebhookReceived } from "@/src/adapter-in/inngest/stripe-workflows";
import { serve } from "inngest/next";
import { inngest } from "../../../src/adapter-in/inngest/client";

export const maxDuration = 300;

export const { GET, POST, PUT } = serve({
  client: inngest,
  functions: [
    onDataSourceInitialized,
    onDataSourceRefreshRequested,
    onDataSourceItemListReceived,
    dataSourceDeleteRequested,
    googleDriveFolderScanInitiated,
    onedriveFolderScanInitiated,
    onKnowledgeInitialized,
    loadKnowledgeChunk,
    refreshDataSources,
    stripeWebhookReceived,
    clerkWebhookReceived,
    deleteUnusedKnowledges,
    dataSourceMigrationRequested,
    onKnowledgeContentRetrieved,
  ],
});
