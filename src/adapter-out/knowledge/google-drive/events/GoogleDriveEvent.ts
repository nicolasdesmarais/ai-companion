export enum GoogleDriveEvent {
  FOLDER_SCAN_INITIATED = "folder.scan.initiated",
}

export interface FolderScanInitiatedEventPayload {
  orgId: string;
  userId: string;
  oauthTokenId: string;
  dataSourceId: string;
  folderId: string;
}
