import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceItemList";
import vectorDatabaseAdapter from "@/src/adapter-out/knowledge/vector-database/VectorDatabaseAdapter";
import {
  DataSourceItemListReceivedPayload,
  DataSourceRefreshRequestedPayload,
  DomainEvent,
  KnowledgeInitializedEventPayload,
} from "@/src/domain/events/domain-event";
import { KnowledgeDto } from "@/src/domain/models/DataSources";
import dataSourceAdapterService from "@/src/domain/services/DataSourceAdapterService";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import dataSourceViewingService from "@/src/domain/services/DataSourceViewingService";
import { KnowledgeIndexStatus } from "@prisma/client";
import { inngest } from "./client";

export const dataSourceInitialized = inngest.createFunction(
  {
    id: "datasource-initialized",
    onFailure: async ({ error, event }) => {
      const { dataSourceId } = event.data.event.data as any;
      console.error(`Failed to initialize data source ${dataSourceId}`, error);
      await dataSourceManagementService.failDataSource(
        dataSourceId,
        error.message
      );
    },
  },
  { event: DomainEvent.DATASOURCE_INITIALIZED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;
    const forRefresh = false;
    const forceRefresh = false;

    const dataSourceItemList = await step.run(
      "get-datasource-item-list",
      async () => {
        return await dataSourceManagementService.getDataSourceItemList(
          dataSourceId,
          forRefresh,
          forceRefresh
        );
      }
    );
    await publishDataSourceItemList(
      dataSourceId,
      dataSourceItemList,
      step,
      forRefresh,
      forceRefresh
    );
  }
);

export const dataSourceRefreshRequested = inngest.createFunction(
  {
    id: "datasource-refresh-requested",
    onFailure: async ({ error, event }) => {
      const { dataSourceId } = event.data.event.data as any;
      console.error(`Failed to refresh data source ${dataSourceId}`, error);
      await dataSourceManagementService.failDataSource(
        dataSourceId,
        error.message
      );
    },
  },
  { event: DomainEvent.DATASOURCE_REFRESH_REQUESTED },
  async ({ event, step }) => {
    const { dataSourceId, forceRefresh } =
      event.data as DataSourceRefreshRequestedPayload;
    const forRefresh = true;

    const dataSourceItemList = await step.run(
      "get-datasource-item-list",
      async () => {
        return await dataSourceManagementService.getDataSourceItemList(
          dataSourceId,
          forRefresh,
          forceRefresh
        );
      }
    );

    await publishDataSourceItemList(
      dataSourceId,
      dataSourceItemList,
      step,
      forRefresh,
      forceRefresh
    );
  }
);

const publishDataSourceItemList = async (
  dataSourceId: string,
  dataSourceItemList: DataSourceItemList | null,
  step: any,
  forRefresh: boolean,
  forceRefresh: boolean
) => {
  if (!dataSourceItemList || dataSourceItemList.items.length === 0) {
    return;
  }

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
};

export const dataSourceItemListReceived = inngest.createFunction(
  {
    id: "datasource-item-list-received",
    onFailure: async ({ error, event }) => {
      const { dataSourceId } = event.data.event
        .data as DataSourceItemListReceivedPayload;
      console.error(
        `Failed to process datasource-item-list-received for data source ${dataSourceId}`,
        error
      );
      await dataSourceManagementService.failDataSource(
        dataSourceId,
        error.message
      );
    },
  },
  { event: DomainEvent.DATASOURCE_ITEM_LIST_RECEIVED },
  async ({ event, step }) => {
    const payload = event.data as DataSourceItemListReceivedPayload;
    const { dataSourceId, dataSourceItemList, forRefresh, forceRefresh } =
      payload;

    const knowledgeList: KnowledgeDto[] = await step.run(
      "upsert-knowledge-list",
      async () => {
        return await dataSourceManagementService.upsertKnowledgeList(
          dataSourceId,
          dataSourceItemList,
          forceRefresh
        );
      }
    );
    const knowledgeIds = knowledgeList.map((k) => k.id);

    if (forRefresh) {
      await step.run("update-datasource-knowledge-associations", async () => {
        await dataSourceManagementService.updateDataSourceKnowledgeAssociations(
          dataSourceId,
          dataSourceItemList,
          knowledgeList
        );
      });
    } else {
      await step.run("create-datasource-knowledge-associations", async () => {
        await dataSourceManagementService.createDataSourceKnowledgeAssociations(
          dataSourceId,
          knowledgeIds
        );
      });
    }

    const knowledgeListToUpdate = knowledgeList.filter(
      (knowledge) => knowledge.indexStatus === KnowledgeIndexStatus.INITIALIZED
    );

    if (knowledgeListToUpdate.length === 0) {
      await dataSourceManagementService.updateDataSourceStatus(dataSourceId);
      return;
    }

    for (const knowledge of knowledgeListToUpdate) {
      const eventPayload: KnowledgeInitializedEventPayload = {
        dataSourceId,
        knowledgeId: knowledge.id,
      };

      await step.sendEvent("knowledge-initialized", {
        name: DomainEvent.KNOWLEDGE_INITIALIZED,
        data: eventPayload,
      });
    }
  }
);

export const knowledgeInitialized = inngest.createFunction(
  {
    id: "knowledge-initialized",
    onFailure: async ({ error, event }) => {
      const { dataSourceId, knowledgeId } = event.data.event.data as any;
      console.error(
        `Failed to initialize knowledge ${knowledgeId} for data source ${dataSourceId}`,
        error
      );
      await dataSourceManagementService.failDataSourceKnowledge(
        knowledgeId,
        error.message
      );
    },
  },
  { event: DomainEvent.KNOWLEDGE_INITIALIZED },
  async ({ event, step }) => {
    const payload = event.data as KnowledgeInitializedEventPayload;
    const { dataSourceId, knowledgeId } = payload;

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
  {
    id: "knowledge-event-received",
    onFailure: async ({ error, event }) => {
      const { dataSourceType, data } = event.data.event.data;
      const dataSourceAdapter =
        dataSourceAdapterService.getDataSourceAdapter(dataSourceType);
      const { knowledgeId } =
        dataSourceAdapter.retrieveOrgAndKnowledgeIdFromEvent(data);

      console.error(
        `Failed to process knowledge-event-received for knowledgeId=${knowledgeId}`,
        error
      );
      await dataSourceManagementService.failDataSourceKnowledge(
        knowledgeId,
        error.message
      );
    },
  },
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
            orgId: knowledgeIndexingResult.orgId,
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
      limit: 2,
    },
    onFailure: async ({ error, event }) => {
      console.error(
        `Failed to load chunk ${event.data.event.data.index} of knowledge ${event.data.event.data.knowledgeIndexingResult.knowledgeId}: ${event.data.error.message}`
      );
      await dataSourceManagementService.failDataSourceKnowledgeChunk(
        event.data.event.data.knowledgeIndexingResult.knowledgeId,
        event.data.event.data.index,
        event.data.error.message
      );
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

export const deleteUnusedKnowledges = inngest.createFunction(
  { id: "delete-unused-knowledges" },
  { cron: "*/10 * * * *" },
  async ({ step }) => {
    const deletedKnowledgeIds = await step.run(
      "delete-unused-knowledges",
      async () => {
        return await dataSourceManagementService.deleteUnusedKnowledges();
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

export const dataSourceMigrationRequested = inngest.createFunction(
  { id: "datasource-migration-requested" },
  { event: DomainEvent.DATASOURCE_MIGRATION_REQUESTED },
  async ({ event, step }) => {
    const dataSourceIds = await step.run(
      "find-datasources-to-refresh",
      async () => {
        return await dataSourceViewingService.findDataSourcesToMigrate();
      }
    );

    await Promise.all(
      dataSourceIds.map((dataSourceId) =>
        step.run("refresh-datasource", async () => {
          await dataSourceManagementService.refreshDataSourceAsSystem(
            dataSourceId,
            true
          );
        })
      )
    );
  }
);
