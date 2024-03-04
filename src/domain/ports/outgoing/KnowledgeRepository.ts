import { KnowledgeChunkStatus, Prisma } from "@prisma/client";
import {
  KnowledgeChunkCounts,
  KnowledgeCounts,
  KnowledgeDto,
  KnowledgeSummary,
} from "../../models/DataSources";

export interface KnowledgeRepository {
  findById(id: string): Promise<KnowledgeDto | null>;
  getById(id: string): Promise<KnowledgeDto>;
  update(id: string, input: Prisma.KnowledgeUpdateInput): Promise<KnowledgeDto>;

  initializeKnowledgeChunks(
    knowledgeId: string,
    chunkCount: number
  ): Promise<void>;

  persistKnowledgeChunkEvents(
    knowledgeId: string,
    chunks: { chunkNumber: number; eventId: string }[]
  ): Promise<void>;

  updateKnowledgeChunkStatus(
    knowledgeId: string,
    chunkNumber: number,
    status: KnowledgeChunkStatus,
    error?: string
  ): Promise<void>;

  getKnowledgeChunkCounts(knowledgeId: string): Promise<KnowledgeChunkCounts>;

  getKnowledgeCounts(dataSourceId: string): Promise<KnowledgeCounts>;

  findDeletedKnowledgeIdsWithBlobStorageUrl(): Promise<string[]>;

  findCompletedKnowledgeWithRelatedInstances(): Promise<string[]>;

  getAiKnowledgeSummary(aiId: string): Promise<KnowledgeSummary>;
}
