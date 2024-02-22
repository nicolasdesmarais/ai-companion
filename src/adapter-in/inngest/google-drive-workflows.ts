import googleDriveDataSourceAdapter from "@/src/adapter-out/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import {
  GoogleDriveEvent,
  GoogleDriveFolderScanInitiatedPayload,
} from "@/src/adapter-out/knowledge/google-drive/events/GoogleDriveEvent";
import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import {
  DataSourceItemListReceivedPayload,
  DomainEvent,
} from "@/src/domain/events/domain-event";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import { inngest } from "./client";

export const googleDriveFolderScanInitiated = inngest.createFunction(
  {
    id: "google-drive-folder-scan-initiated",
    onFailure: async ({ error, event }) => {
      const { dataSourceId } = event.data.event.data as any;
      console.error(
        `Failed to scan Google Drive folder for data source ${dataSourceId}`,
        error
      );
      await dataSourceManagementService.failDataSource(
        dataSourceId,
        error.message
      );
    },
  },
  { event: GoogleDriveEvent.GOOGLE_DRIVE_FOLDER_SCAN_INITIATED },
  async ({ event, step }) => {
    const payload = event.data as GoogleDriveFolderScanInitiatedPayload;
    const {
      orgId,
      userId,
      oauthTokenId,
      dataSourceId,
      folderId,
      forRefresh,
      forceRefresh,
    } = payload;

    const dataSourceItemList: DataSourceItemList = await step.run(
      "get-datasource-item-list-from-folder",
      async () => {
        return await googleDriveDataSourceAdapter.getDataSourceItemListFromFolder(
          orgId,
          userId,
          oauthTokenId,
          dataSourceId,
          folderId,
          forRefresh,
          forceRefresh
        );
      }
    );

    const eventPayload: DataSourceItemListReceivedPayload = {
      dataSourceId,
      dataSourceItemList,
      forRefresh,
      forceRefresh,
    };
    await step.sendEvent("datasource-item-list-received", {
      name: DomainEvent.DATASOURCE_ITEM_LIST_RECEIVED,
      data: eventPayload,
    });
  }
);
