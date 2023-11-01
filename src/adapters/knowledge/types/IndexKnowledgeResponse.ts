import { KnowledgeIndexStatus } from "@prisma/client";

export interface IndexKnowledgeResponse {
  indexStatus: KnowledgeIndexStatus;
  metadata?: any;
  blobUrl?: string;
}
