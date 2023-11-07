import { DomainEvent } from "@/src/domain/events/domain-event";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { inngest } from "./client";

export const dataSourceInitialized = inngest.createFunction(
  { id: "datasource-initialized" },
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

export const knowledgeEventReceived = inngest.createFunction(
  { id: "knowledge-event-received" },
  { event: DomainEvent.KNOWLEDGE_EVENT_RECEIVED },
  async ({ event, step }) => {
    const { dataSourceType, data } = event.data;

    await step.run("handle-knowledge-event-received", async () => {
      await dataSourceService.handleKnowledgeEventReceived(
        dataSourceType,
        data
      );
    });
  }
);

export const pollIndexingDataSources = inngest.createFunction(
  { id: "poll-indexing-datasources" },
  { cron: "0 * * * *" },
  async ({ step }) => {
    const dataSources = await step.run("get-indexing-datasources", async () => {
      return await dataSourceService.getIndexingDataSources();
    });

    const steps = [];
    for (const dataSource of dataSources) {
      steps.push(
        step.run("poll-datasource", async () => {
          await dataSourceService.pollDataSource(
            dataSource.id,
            dataSource.type
          );
        })
      );
    }

    if (steps.length > 0) {
      await Promise.all(steps);
    }
  }
);
