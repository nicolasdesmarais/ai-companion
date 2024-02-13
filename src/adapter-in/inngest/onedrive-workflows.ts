import msftDataSourceAdapter, {
  MsftEvent,
} from "@/src/adapter-out/knowledge/msft/MsftDataSourceAdapter";
import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceItemList";
import {
  DataSourceItemListReceivedPayload,
  DomainEvent,
} from "@/src/domain/events/domain-event";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import { inngest } from "./client";

export const onedriveFolderScanInitiated = inngest.createFunction(
  {
    id: "onedrive-folder-scan-initiated",
    onFailure: async ({ error, event }) => {
      const { dataSourceId } = event.data.event.data as any;
      console.error(
        `Failed to scan onedrive folder for data source ${dataSourceId}`,
        error
      );
      await dataSourceManagementService.failDataSource(
        dataSourceId,
        event.data.error.message
      );
    },
  },
  { event: MsftEvent.ONEDRIVE_FOLDER_SCAN_INITIATED },
  async ({ event, step }) => {
    console.log("onedriveFolderScanInitiated", event.data);
    const { userId, oauthTokenId, dataSourceId, folderId, forRefresh } =
      event.data;

    const dataSourceItemList: DataSourceItemList = await step.run(
      "get-datasource-item-list-from-folder",
      async () => {
        return await msftDataSourceAdapter.getDataSourceItemListFromFolder(
          userId,
          oauthTokenId,
          dataSourceId,
          folderId,
          forRefresh
        );
      }
    );

    const eventPayload: DataSourceItemListReceivedPayload = {
      dataSourceId,
      dataSourceItemList,
      forRefresh,
    };
    await step.sendEvent("datasource-item-list-received", {
      name: DomainEvent.DATASOURCE_ITEM_LIST_RECEIVED,
      data: eventPayload,
    });
  }
);
