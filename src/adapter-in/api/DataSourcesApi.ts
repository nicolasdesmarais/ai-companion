import { DataSourceDto } from "@/src/domain/models/DataSources";
import { DataSourceRefreshPeriod } from "@prisma/client";
import mime from "mime";

export interface ListDataSourcesResponse {
  data: DataSourceDto[];
}
export interface CreateApiDataSourceRequest {
  name: string;
  data: any;
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

export function isSupportedFileType(fileName: string) {
  const fileType = mime.lookup(fileName);
  return Object.values(FileType).includes(fileType as FileType);
}

export enum FileType {
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

export interface UpdateDataSourceRequest {
  refreshPeriod: DataSourceRefreshPeriod | null;
  ais: string[];
}
