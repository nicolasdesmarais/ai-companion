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

    if (knowledgeIndexingResult.result.chunkCount) {
      const events = [];
      for (let i = 0; i < knowledgeIndexingResult.result.chunkCount; i++) {
        events.push({
          name: DomainEvent.KNOWLEDGE_CHUNK_RECEIVED,
          data: {
            knowledgeIndexingResult,
            dataSourceType,
            index: i,
          },
        });
      }
      await step.sendEvent("fan-out-knowledge-chunks", events);
    }
  }
);

export const loadKnowledgeChunk = inngest.createFunction(
  {
    id: "knowledge-chunk-received",
    concurrency: {
      limit: 2,
    },
  },
  { event: DomainEvent.KNOWLEDGE_CHUNK_RECEIVED },
  async ({ event, step }) => {
    const indexingResult = await dataSourceService.loadKnowledgeResult(
      event.data.dataSourceType,
      event.data.knowledgeIndexingResult.knowledgeId,
      event.data.knowledgeIndexingResult.result,
      event.data.index
    );
    await dataSourceService.persistIndexingResult(
      event.data.knowledgeIndexingResult.knowledgeId,
      indexingResult,
      event.data.knowledgeIndexingResult.result.chunkCount
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
