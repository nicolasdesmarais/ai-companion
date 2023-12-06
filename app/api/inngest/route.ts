import {
  dataSourceInitialized,
  knowledgeEventReceived,
  knowledgeInitialized,
  loadKnowledgeChunk,
} from "@/src/adapter-out/inngest/datasource-workflows";
import { serve } from "inngest/next";
import { inngest } from "../../../src/adapter-out/inngest/client";

export const maxDuration = 300;

export const { GET, POST, PUT } = serve({
  client: inngest,
  functions: [
    dataSourceInitialized,
    knowledgeInitialized,
    knowledgeEventReceived,
    loadKnowledgeChunk,
  ],
});
