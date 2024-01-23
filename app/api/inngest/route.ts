import {
  dataSourceDeleteRequested,
  dataSourceInitialized,
  dataSourceItemListReceived,
  dataSourceRefreshRequested,
  knowledgeEventReceived,
  knowledgeInitialized,
  loadKnowledgeChunk,
  refreshDataSources,
} from "@/src/adapter-in/inngest/datasource-workflows";
import { googleDriveFolderScanInitiated } from "@/src/adapter-in/inngest/google-drive-workflows";
import { serve } from "inngest/next";
import { inngest } from "../../../src/adapter-in/inngest/client";

export const maxDuration = 300;

export const { GET, POST, PUT } = serve({
  client: inngest,
  functions: [
    dataSourceInitialized,
    dataSourceRefreshRequested,
    dataSourceItemListReceived,
    dataSourceDeleteRequested,
    googleDriveFolderScanInitiated,
    knowledgeInitialized,
    knowledgeEventReceived,
    loadKnowledgeChunk,
    refreshDataSources,
  ],
});
