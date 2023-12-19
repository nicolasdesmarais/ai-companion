import { drive_v3 } from "googleapis";
import { DataSourceItem } from "../../types/DataSourceItemList";
import { GoogleDriveFileMetadata } from "../types/GoogleDriveFileMetaData";

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
