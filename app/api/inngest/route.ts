import {
  dataSourceInitialized,
  dataSourceItemListReceived,
  knowledgeEventReceived,
  knowledgeInitialized,
  loadKnowledgeChunk,
} from "@/src/adapter-in/inngest/datasource-workflows";
import { folderScanInitiated } from "@/src/adapter-in/inngest/google-drive-workflows";
import { serve } from "inngest/next";
import { inngest } from "../../../src/adapter-in/inngest/client";

export const maxDuration = 300;

export const { GET, POST, PUT } = serve({
  client: inngest,
  functions: [
    dataSourceInitialized,
    dataSourceItemListReceived,
    knowledgeInitialized,
    knowledgeEventReceived,
    loadKnowledgeChunk,
    folderScanInitiated,
  ],
});
