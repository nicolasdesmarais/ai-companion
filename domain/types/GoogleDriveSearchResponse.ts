export interface GoogleDriveSearchResponse {
  files: GoogleDriveFile[];
}

export interface GoogleDriveFile {
  id: string;
  name: string;
  type: string;
}
