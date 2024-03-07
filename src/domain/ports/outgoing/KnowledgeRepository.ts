import { KnowledgeChunkIndexes } from "@/src/adapter-out/knowledge/types/KnowledgeChunkTypes";
import { KnowledgeChunkStatus, Prisma } from "@prisma/client";
import {
  KnowledgeChunkCounts,
  KnowledgeChunkDto,
  KnowledgeCounts,
  KnowledgeDto,
  KnowledgeSummary,
} from "../../models/DataSources";

export interface KnowledgeRepository {
  findById(id: string): Promise<KnowledgeDto | null>;
  getById(id: string): Promise<KnowledgeDto>;
  update(id: string, input: Prisma.KnowledgeUpdateInput): Promise<KnowledgeDto>;

  getKnowledgeChunkByNumber(
    knowledgeId: string,
    chunkNumber: number
  ): Promise<KnowledgeChunkDto>;

  persistKnowledgeChunks(
    knowledgeId: string,
    knowledgeChunkIndexes: KnowledgeChunkIndexes[]
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

  findDeletedKnowledgeIdsWithVectorStorage(limit?: number): Promise<string[]>;

  findCompletedKnowledgeWithRelatedInstances(): Promise<string[]>;

  getAiKnowledgeSummary(aiId: string): Promise<KnowledgeSummary>;
}
