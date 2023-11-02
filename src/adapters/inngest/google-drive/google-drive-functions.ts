import aiService from "@/src/domain/services/AIService";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { DataSourceType } from "@prisma/client";
import { inngest } from "../client";

export const googleDriveDataSourceCreationRequested = inngest.createFunction(
  { id: "google-drive-datasource" },
  { event: "google-drive/datasource.creation.requested" },
  async ({ event, step }) => {
    const dataSourceId = await step.run("create-data-source", async () => {
      return await dataSourceService.createDataSource(
        event.data.orgId,
        event.data.userId,
        event.data.input.name,
        DataSourceType.GOOGLE_DRIVE,
        event.data.input
      );
    });

    await step.run("create-ai-data-source", async () => {
      await aiService.createAIDataSource(event.data.aiId, dataSourceId);
    });
  }
);
