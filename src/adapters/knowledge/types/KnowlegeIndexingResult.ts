export interface KnowledgeIndexingResult {
  status: KnowledgeIndexingResultStatus;
  blobUrl?: string;
}

export enum KnowledgeIndexingResultStatus {
  SUCCESSFUL,
  PARTIAL,
  FAILED,
}
