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
  FOLDER = "application/vnd.google-apps.folder",
  UNKNOWN = "unknown",
}
