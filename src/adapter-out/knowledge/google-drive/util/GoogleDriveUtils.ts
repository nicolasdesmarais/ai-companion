import { drive_v3 } from "googleapis";
import { DataSourceItem } from "../../types/DataSourceItemList";
import { GoogleDriveFileMetadata } from "../types/GoogleDriveFileMetaData";

export const MIME_TYPE_TEXT = "text/plain";
export const MIME_TYPE_CSV = "text/csv";
export const MIME_TYPE_EPUB = "application/epub+zip";
export const MIME_TYPE_PDF = "application/pdf";
export const MIME_TYPE_MARKDOWN = "text/markdown";
export const MIME_TYPE_GOOGLE_DOC = "application/vnd.google-apps.document";
export const MIME_TYPE_GOOGLE_SHEETS =
  "application/vnd.google-apps.spreadsheet";
export const MIME_TYPE_GOOGLE_SLIDES =
  "application/vnd.google-apps.presentation";
export const MIME_TYPE_DOCX =
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document";

export const SUPPORTED_MIME_TYPES = [
  MIME_TYPE_TEXT,
  MIME_TYPE_CSV,
  MIME_TYPE_EPUB,
  MIME_TYPE_PDF,
  MIME_TYPE_MARKDOWN,
  MIME_TYPE_GOOGLE_DOC,
  MIME_TYPE_GOOGLE_SHEETS,
  MIME_TYPE_GOOGLE_SLIDES,
  MIME_TYPE_DOCX,
];

export const FOLDER_MIME_TYPE = "application/vnd.google-apps.folder";

export function mapGoogleDriveFileToDataSourceItem(
  file: drive_v3.Schema$File
): DataSourceItem {
  const metadata: GoogleDriveFileMetadata = {
    fileId: file.id ?? "",
    fileName: file.name ?? "",
    mimeType: file.mimeType ?? "",
    modifiedTime: file.modifiedTime ?? "",
  };

  const uniqueId = `${metadata.fileId}`;
  return {
    name: file.name ?? "",
    uniqueId,
    metadata,
  };
}
