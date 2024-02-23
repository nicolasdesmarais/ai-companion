import { onApifyWebhookReceived } from "@/src/adapter-in/inngest/apify-workflows";
import { clerkWebhookReceived } from "@/src/adapter-in/inngest/clerk-workflows";
import {
  dataSourceMigrationRequested,
  deleteUnusedKnowledges,
  dataSourceDeleteRequested as onDataSourceDeleteRequested,
  onDataSourceInitialized,
  onDataSourceItemListReceived,
  onDataSourceRefreshRequested,
  onKnowledgeChunkReceived,
  onKnowledgeContentRetrieved,
  onKnowledgeIndexingCompleted,
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
    onDataSourceDeleteRequested,
    googleDriveFolderScanInitiated,
    onedriveFolderScanInitiated,
    onKnowledgeInitialized,
    onKnowledgeChunkReceived,
    onKnowledgeContentRetrieved,
    onKnowledgeIndexingCompleted,
    refreshDataSources,
    stripeWebhookReceived,
    clerkWebhookReceived,
    deleteUnusedKnowledges,
    dataSourceMigrationRequested,
    onApifyWebhookReceived,
  ],
});
