import dataSourceService from "@/src/domain/services/DataSourceService";
import { inngest } from "../client";

export const dataSourcePersisted = inngest.createFunction(
  { id: "datasource-persisted" },
  { event: DomainEvent.DATASOURCE_INITIALIZED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;

    await step.run("get-knowledge-list", async () => {
      return await dataSourceService.createDataSourceKnowledgeList(
        dataSourceId
      );
    });

    await step.run("index-datasource-knowlege", async () => {
      await dataSourceService.indexDataSourceKnowledge(dataSourceId);
    });
  }
);
