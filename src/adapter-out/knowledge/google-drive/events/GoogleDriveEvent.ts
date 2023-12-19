export enum GoogleDriveEvent {
  GOOGLE_DRIVE_FOLDER_SCAN_INITIATED = "google.drive.folder.scan.initiated",
}

export interface GoogleDriveFolderScanInitiatedPayload {
  orgId: string;
  userId: string;
  oauthTokenId: string;
  dataSourceId: string;
  folderId: string;
}
