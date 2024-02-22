export interface ChunkLoadingResult {
  chunkNumber: number;
  docIds: string[];
  status: ChunkLoadingResultStatus;
}

export enum ChunkLoadingResultStatus {
  SUCCESSFUL,
  FAILED,
}
