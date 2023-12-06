export interface KnowledgeIndexingResult {
  status: KnowledgeIndexingResultStatus;
  blobUrl?: string;
  chunkCount?: number;
}

export enum KnowledgeIndexingResultStatus {
  SUCCESSFUL,
  PARTIAL,
  FAILED,
}
