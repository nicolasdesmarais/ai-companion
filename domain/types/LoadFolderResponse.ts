export interface LoadFolderResponse {
  folders: FolderResponse[];
  knowledgeIds: string[];
}

export interface FolderResponse {
  id: string;
  name: string;
  files?: FileResponse[];
}

export interface FileResponse {
  id: string;
  name: string;
  type: string;
}
