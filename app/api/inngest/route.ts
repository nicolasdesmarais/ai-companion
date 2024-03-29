import {
  onApifyActorRunRequested,
  onApifyWebhookReceived,
} from "@/src/adapter-in/inngest/apify-workflows";
import { clerkWebhookReceived } from "@/src/adapter-in/inngest/clerk-workflows";
import {
  deleteBlobStorage,
  deleteRelatedKnowledgeInstances,
  deleteUnusedKnowledges,
  deleteVectorDBStorage,
  onDataSourceDeleteRequested,
  onDataSourceInitialized,
  onDataSourceItemListReceived,
  onDataSourceRefreshRequested,
  onKnowledgeChunkReceived,
  onKnowledgeContentRetrieved,
  onKnowledgeDeleted,
  onKnowledgeIndexingCompletedSuccessfully,
  onKnowledgeInitialized,
  onKnowledgeRetryRequested,
  refreshDataSources,
} from "@/src/adapter-in/inngest/datasource-workflows";
import { onFunctionFailed } from "@/src/adapter-in/inngest/error-workflows";
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
    onFunctionFailed,
    googleDriveFolderScanInitiated,
    onedriveFolderScanInitiated,
    onKnowledgeInitialized,
    onKnowledgeChunkReceived,
    onKnowledgeContentRetrieved,
    onKnowledgeIndexingCompletedSuccessfully,
    onKnowledgeDeleted,
    onKnowledgeRetryRequested,
    refreshDataSources,
    stripeWebhookReceived,
    clerkWebhookReceived,
    deleteUnusedKnowledges,
    onApifyActorRunRequested,
    onApifyWebhookReceived,
    deleteBlobStorage,
    deleteRelatedKnowledgeInstances,
    deleteVectorDBStorage,
  ],
});
