import {
  dataSourceInitialized,
  knowledgeEventReceived,
  loadKnowledgeChunk,
} from "@/src/adapter/inngest/datasource-workflows";
import { serve } from "inngest/next";
import { inngest } from "../../../src/adapter/inngest/client";

export const maxDuration = 300;

export const { GET, POST, PUT } = serve({
  client: inngest,
  functions: [
    dataSourceInitialized,
    knowledgeEventReceived,
    loadKnowledgeChunk,
  ],
});
