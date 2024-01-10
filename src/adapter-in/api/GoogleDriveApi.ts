import { DataSourceRefreshPeriod } from "@prisma/client";

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

export function getLabelFromFileType(fileType: FileType) {
  switch (fileType) {
    case FileType.TEXT:
      return "Text";
    case FileType.CSV:
      return "CSV";
    case FileType.EPUB:
      return "EPUB";
    case FileType.PDF:
      return "PDF";
    case FileType.DOCX:
      return "Document";
    case FileType.GOOGLE_DOC:
      return "Google Document";
    case FileType.GOOGLE_SHEETS:
      return "Google Sheet";
    case FileType.GOOGLE_SLIDES:
      return "Google Slides";
    case FileType.FOLDER:
      return "Folder";
    default:
      return "Unknown";
  }
}

enum FileType {
  TEXT = "text/plain",
  CSV = "text/csv",
  EPUB = "application/epub+zip",
  PDF = "application/pdf",
  DOCX = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  GOOGLE_DOC = "application/vnd.google-apps.document",
  GOOGLE_SHEETS = "application/vnd.google-apps.spreadsheet",
  GOOGLE_SLIDES = "application/vnd.google-apps.presentation",
  FOLDER = "application/vnd.google-apps.folder",
  UNKNOWN = "unknown",
}
