import { KnowledgeIndexStatus } from "@prisma/client";

export interface IndexKnowledgeResponse {
  indexStatus: KnowledgeIndexStatus;
  documentCount?: number;
  tokenCount?: number;
  metadata?: any;
  blobUrl?: string;
  userId?: string;
  events?: any[];
}
