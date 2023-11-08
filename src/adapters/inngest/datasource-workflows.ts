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

    const knowledgeIndexingResult = await step.run(
      "handle-knowledge-event-received",
      async () => {
        return await dataSourceService.getKnowledgeResultFromEvent(
          dataSourceType,
          data
        );
      }
    );

    const indexingResult = await step.run("load-knowledge-result", async () => {
      return await dataSourceService.loadKnowledgeResult(
        dataSourceType,
        knowledgeIndexingResult.knowledgeId,
        knowledgeIndexingResult.result
      );
    });

    await step.run("persist-knowledge-indexing-result", async () => {
      await dataSourceService.persistIndexingResult(
        knowledgeIndexingResult.knowledgeId,
        indexingResult
      );
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
