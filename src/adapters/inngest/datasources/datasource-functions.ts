import dataSourceService from "@/src/domain/services/DataSourceService";
import { inngest } from "../client";

export const dataSourcePersisted = inngest.createFunction(
  { id: "datasource-persisted" },
  { event: "datasources/datasource.persisted" },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;

    await step.run("get-knowledge-list", async () => {
      return await dataSourceService.getDataSourceKnowledgeList(dataSourceId);
    });

    await step.run("index-datasource-knowlege", async () => {
      await dataSourceService.indexDataSourceKnowlege(dataSourceId);
    });
  }
);
