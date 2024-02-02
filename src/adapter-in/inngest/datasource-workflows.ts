import vectorDatabaseAdapter from "@/src/adapter-out/knowledge/vector-database/VectorDatabaseAdapter";
import {
  DataSourceItemListReceivedPayload,
  DomainEvent,
} from "@/src/domain/events/domain-event";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import { inngest } from "./client";

export const dataSourceInitialized = inngest.createFunction(
  { id: "datasource-initialized" },
  { event: DomainEvent.DATASOURCE_INITIALIZED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;

    await step.run("create-knowledge-list", async () => {
      await dataSourceManagementService.createDataSourceKnowledgeList(
        dataSourceId
      );
    });
  }
);

export const dataSourceRefreshRequested = inngest.createFunction(
  { id: "datasource-refresh-requested" },
  { event: DomainEvent.DATASOURCE_REFRESH_REQUESTED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;

    await step.run("update-knowledge-list", async () => {
      await dataSourceManagementService.refreshDataSourceKnowledgeList(
        dataSourceId
      );
    });
  }
);

export const dataSourceItemListReceived = inngest.createFunction(
  { id: "datasource-item-list-received" },
  { event: DomainEvent.DATASOURCE_ITEM_LIST_RECEIVED },
  async ({ event, step }) => {
    const payload = event.data as DataSourceItemListReceivedPayload;
    const { dataSourceId, dataSourceItemList } = payload;

    await step.run("on-datasource-item-list-received", async () => {
      await dataSourceManagementService.onDataSourceItemListReceived(
        dataSourceId,
        dataSourceItemList
      );
    });
  }
);

export const knowledgeInitialized = inngest.createFunction(
  { id: "knowledge-initialized" },
  { event: DomainEvent.KNOWLEDGE_INITIALIZED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;
    const knowledgeId = event.data.knowledgeId;

    const result = await step.run("index-knowledge", async () => {
      return await dataSourceManagementService.indexDataSourceKnowledge(
        dataSourceId,
        knowledgeId
      );
    });
    if (result?.events?.length && result?.events?.length > 0) {
      while (result.events.length) {
        const eventBatch = result.events.splice(0, INGEST_EVENT_MAX);
        await step.sendEvent("fan-out-knowledge-chunks", eventBatch);
      }
    }

    const relatedKnowledgeIds = await step.run("delete-knowledge", async () => {
      return await dataSourceManagementService.deleteRelatedKnowledgeInstances(
        knowledgeId
      );
    });

    await Promise.all(
      relatedKnowledgeIds.map((knowledgeId) =>
        step.run("delete-vectordb-knowledge", async () => {
          await vectorDatabaseAdapter.deleteKnowledge(knowledgeId);
        })
      )
    );
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
        return await dataSourceManagementService.getKnowledgeResultFromEvent(
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
      const indexingResult =
        await dataSourceManagementService.loadKnowledgeResult(
          event.data.orgId,
          event.data.dataSourceType,
          event.data.knowledgeIndexingResult.knowledgeId,
          event.data.knowledgeIndexingResult.result,
          event.data.index
        );
      await dataSourceManagementService.persistIndexingResult(
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

export const refreshDataSources = inngest.createFunction(
  { id: "refresh-datasources" },
  { cron: "0 0 * * *" },
  async ({ step }) => {
    const dataSourceIds = await step.run(
      "find-datasources-to-refresh",
      async () => {
        return await dataSourceManagementService.findDataSourcesToRefresh();
      }
    );

    await Promise.all(
      dataSourceIds.map((dataSourceId) =>
        step.run("refresh-datasource", async () => {
          await dataSourceManagementService.refreshDataSourceAsSystem(
            dataSourceId
          );
        })
      )
    );
  }
);

export const pollIndexingDataSources = inngest.createFunction(
  { id: "poll-indexing-datasources" },
  { cron: "0 * * * *" },
  async ({ step }) => {
    const dataSources = await step.run("get-indexing-datasources", async () => {
      return await dataSourceManagementService.getIndexingDataSources();
    });

    const steps = [];
    for (const dataSource of dataSources) {
      steps.push(
        step.run("poll-datasource", async () => {
          await dataSourceManagementService.pollDataSource(
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

export const dataSourceDeleteRequested = inngest.createFunction(
  { id: "datasource-delete-requested" },
  { event: DomainEvent.DATASOURCE_DELETE_REQUESTED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;

    const deletedKnowledgeIds = await step.run(
      "delete-data-source",
      async () => {
        return await dataSourceManagementService.deleteDataSource(dataSourceId);
      }
    );

    await Promise.all(
      deletedKnowledgeIds.map((knowledgeId) =>
        step.run("delete-vectordb-knowledge", async () => {
          await vectorDatabaseAdapter.deleteKnowledge(knowledgeId);
        })
      )
    );
  }
);
