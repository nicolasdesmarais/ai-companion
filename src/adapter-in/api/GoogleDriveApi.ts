import { DataSourceRefreshPeriod } from "@prisma/client";
import { FileType } from "./DataSourcesApi";

export interface CreateGoogleDriveKnowledgeRequest {
  oauthTokenId: string;
  fileId: string;
  filename: string;
  dataRefreshPeriod?: DataSourceRefreshPeriod;
}

export interface GoogleDriveSearchRequest {
  oauthTokenId: string;
  searchTerms: string[];
}

export interface GoogleDriveSearchResponse {
  files: GoogleDriveFile[];
}

export interface GoogleDriveFile {
  id: string;
  name: string;
  type: FileType;
  owner: string;
  modifiedTime: string;
}

export function mapMimeTypeToEnum(
  mimeType: string | null | undefined
): FileType {
  switch (mimeType) {
    case FileType.TEXT:
      return FileType.TEXT;
    case FileType.CSV:
      return FileType.CSV;
    case FileType.EPUB:
      return FileType.EPUB;
    case FileType.PDF:
      return FileType.PDF;
    case FileType.DOCX:
      return FileType.DOCX;
    case FileType.GOOGLE_DOC:
      return FileType.GOOGLE_DOC;
    case FileType.GOOGLE_SHEETS:
      return FileType.GOOGLE_SHEETS;
    case FileType.GOOGLE_SLIDES:
      return FileType.GOOGLE_SLIDES;
    case FileType.FOLDER:
      return FileType.FOLDER;
    default:
      return FileType.UNKNOWN;
  }
}
