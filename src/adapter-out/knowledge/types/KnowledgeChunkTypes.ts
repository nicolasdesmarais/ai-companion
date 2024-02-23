import { KnowledgeChunkStatus } from "@prisma/client";

export interface KnowledgeChunkEvent {
  chunkNumber: number;
  eventId: string;
}

export interface ChunkLoadingResult {
  chunkNumber: number;
  docIds?: string[];
  status: KnowledgeChunkStatus;
  error?: string;
}
