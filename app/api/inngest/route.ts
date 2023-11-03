import {
  dataSourceInitialized,
  knowledgeEventReceived,
} from "@/src/adapters/inngest/datasource-workflows";
import { serve } from "inngest/next";
import { inngest } from "../../../src/adapters/inngest/client";

export const maxDuration = 300;

export const { GET, POST, PUT } = serve({
  client: inngest,
  functions: [dataSourceInitialized, knowledgeEventReceived],
});
