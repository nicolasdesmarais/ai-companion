import { DomainEvent } from "@/src/domain/events/domain-event";
import dataSourceService from "@/src/domain/services/DataSourceService";
import { inngest } from "./client";

export const dataSourceInitialized = inngest.createFunction(
  { id: "datasource-initialized" },
  { event: DomainEvent.DATASOURCE_INITIALIZED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;

    const knowledgeIdList = await step.run("get-knowledge-list", async () => {
      return await dataSourceService.createDataSourceKnowledgeList(
        dataSourceId
      );
    });

    let events = [];
    for (const knowledgeId of knowledgeIdList) {
      events.push({
        name: DomainEvent.KNOWLEDGE_INITIALIZED,
        data: {
          dataSourceId,
          knowledgeId,
        },
      });

      if (events.length >= INGEST_EVENT_MAX) {
        await step.sendEvent("fan-out-knowledge-initialized", events);
        events = [];
      }
    }

    if (events.length >= 0) {
      await step.sendEvent("fan-out-knowledge-initialized", events);
      events = [];
    }
  }
);

export const knowledgeInitialized = inngest.createFunction(
  { id: "knowledge-initialized" },
  { event: DomainEvent.KNOWLEDGE_INITIALIZED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;
    const knowledgeId = event.data.knowledgeId;

    await step.run("index-knowledge", async () => {
      await dataSourceService.indexDataSourceKnowledge(
        dataSourceId,
        knowledgeId
      );
    });
  }
);

const INGEST_EVENT_MAX = 2000;

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
      let events = [];
      for (let i = 0; i < knowledgeIndexingResult.result.chunkCount; i++) {
        events.push({
          name: DomainEvent.KNOWLEDGE_CHUNK_RECEIVED,
          data: {
            knowledgeIndexingResult,
            dataSourceType,
            index: i,
          },
        });
        if (events.length >= INGEST_EVENT_MAX) {
          await step.sendEvent("fan-out-knowledge-chunks", events);
          events = [];
        }
      }
      if (events.length > 0) {
        await step.sendEvent("fan-out-knowledge-chunks", events);
      }
      return { count: events.length };
    }
  }
);

export const loadKnowledgeChunk = inngest.createFunction(
  {
    id: "knowledge-chunk-received",
    concurrency: {
      limit: 1,
    },
  },
  { event: DomainEvent.KNOWLEDGE_CHUNK_RECEIVED },
  async ({ event }) => {
    try {
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
    } catch (e) {
      console.error(
        "[KNOWLEDGE CHUNK]",
        event.data.knowledgeIndexingResult.knowledgeId,
        event.data.index,
        e
      );
      throw (new Error("Error loading knowledge chunk"), e);
    }
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
