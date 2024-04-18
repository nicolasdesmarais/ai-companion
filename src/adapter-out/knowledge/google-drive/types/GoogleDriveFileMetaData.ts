export interface GoogleDriveFileMetadata {
  fileId: string;
  fileName: string;
  parentFolderId?: string;
  mimeType: string;
  modifiedTime?: string;
}
