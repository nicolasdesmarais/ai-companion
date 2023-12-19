import googleDriveDataSourceAdapter from "@/src/adapter-out/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import {
  FolderScanInitiatedEventPayload,
  GoogleDriveEvent,
} from "@/src/adapter-out/knowledge/google-drive/events/GoogleDriveEvent";
import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceItemList";
import {
  DataSourceItemListReceivedPayload,
  DomainEvent,
} from "@/src/domain/events/domain-event";
import { inngest } from "./client";

export const folderScanInitiated = inngest.createFunction(
  { id: "folder-scan-initiated" },
  { event: GoogleDriveEvent.FOLDER_SCAN_INITIATED },
  async ({ event, step }) => {
    const payload = event.data as FolderScanInitiatedEventPayload;
    const { orgId, userId, oauthTokenId, dataSourceId, folderId } = payload;

    const dataSourceItemList: DataSourceItemList = await step.run(
      "scan-folder",
      async () => {
        return await googleDriveDataSourceAdapter.scanFolder(
          orgId,
          userId,
          oauthTokenId,
          dataSourceId,
          folderId
        );
      }
    );

    const eventPayload: DataSourceItemListReceivedPayload = {
      dataSourceId,
      dataSourceItemList,
    };
    await step.sendEvent("datasource-item-list-received", {
      name: DomainEvent.DATASOURCE_ITEM_LIST_RECEIVED,
      data: eventPayload,
    });
  }
);
