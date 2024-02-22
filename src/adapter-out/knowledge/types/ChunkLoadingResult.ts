export interface ChunkLoadingResult {
  chunkNumber: number;
  chunkCount: number;
  docIds: string[];
  status: ChunkLoadingResultStatus;
}

export enum ChunkLoadingResultStatus {
  SUCCESSFUL,
  FAILED,
}
