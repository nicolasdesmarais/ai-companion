import { DataSourceDto } from "@/src/domain/models/DataSources";
import { DataSourceRefreshPeriod } from "@prisma/client";
import mime from "mime-types";

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
      return "Word Document";
    case FileType.GOOGLE_DOC:
      return "Google Document";
    case FileType.GOOGLE_SHEETS:
      return "Google Sheet";
    case FileType.GOOGLE_SLIDES:
      return "Google Slides";
    case FileType.MARKDOWN:
      return "Markdown";
    case FileType.JSON:
      return "JSON";
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
  MARKDOWN = "text/markdown",
  JSON = "application/json",
  UNKNOWN = "unknown",
}

export interface UpdateDataSourceRequest {
  refreshPeriod: DataSourceRefreshPeriod | null;
  ais: string[];
}

export enum MsftConvertibleFileType {
  EML = "message/rfc822",
  MSG = "application/vnd.ms-outlook",
  ODP = "application/vnd.oasis.opendocument.presentation",
  ODS = "application/vnd.oasis.opendocument.spreadsheet",
  ODT = "application/vnd.oasis.opendocument.text",
  PPSX = "application/vnd.openxmlformats-officedocument.presentationml.slideshow",
  PPTX = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
  RTF = "application/rtf",
  XLS = "application/vnd.ms-excel",
  XLSM = "application/vnd.ms-excel.sheet.macroenabled.12",
  XLSX = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
}

export const isMsftConvertible = (filename: string): boolean => {
  const filetype = mime.lookup(filename);
  return !!(
    filetype &&
    Object.values(MsftConvertibleFileType).includes(
      filetype as MsftConvertibleFileType
    )
  );
};

export const getMsftLabelFromFileType = (fileType: string) => {
  switch (fileType) {
    case MsftConvertibleFileType.EML:
      return "Email";
    case MsftConvertibleFileType.MSG:
      return "Outlook";
    case MsftConvertibleFileType.ODP:
      return "Presentation";
    case MsftConvertibleFileType.ODS:
      return "Spreadsheet";
    case MsftConvertibleFileType.ODT:
      return "Open Word";
    case MsftConvertibleFileType.PPSX:
      return "Slide Show";
    case MsftConvertibleFileType.PPTX:
      return "PowerPoint";
    case MsftConvertibleFileType.RTF:
      return "Rich Text";
    case MsftConvertibleFileType.XLS:
      return "Legacy Excel";
    case MsftConvertibleFileType.XLSM:
      return "Excel (Macro)";
    case MsftConvertibleFileType.XLSX:
      return "Excel";
    default:
      return getLabelFromFileType(fileType as FileType);
  }
};
