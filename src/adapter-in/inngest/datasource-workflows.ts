import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import { ChunkLoadingResult } from "@/src/adapter-out/knowledge/types/KnowledgeChunkTypes";
import vectorDatabaseAdapter from "@/src/adapter-out/knowledge/vector-database/VectorDatabaseAdapter";
import {
  DataSourceInitializedPayload,
  DataSourceItemListReceivedPayload,
  DataSourceRefreshRequestedPayload,
  DomainEvent,
  KnowledgeChunkReceivedPayload,
  KnowledgeContentReceivedPayload as KnowledgeContentRetrievedPayload,
  KnowledgeIndexingCompletedPayload,
  KnowledgeInitializedEventPayload,
} from "@/src/domain/events/domain-event";
import {
  KnowledgeDto,
  knowldedgeEndStatuses as knowledgeEndStatuses,
} from "@/src/domain/models/DataSources";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import dataSourceViewingService from "@/src/domain/services/DataSourceViewingService";
import { KnowledgeChunkStatus, KnowledgeIndexStatus } from "@prisma/client";
import { inngest } from "./client";

export const onDataSourceInitialized = inngest.createFunction(
  {
    id: "on-datasource-initialized",
    onFailure: async ({ error, event }) => {
      const { dataSourceId } = event.data.event
        .data as DataSourceInitializedPayload;
      console.error(`Failed to initialize data source ${dataSourceId}`, error);
      await dataSourceManagementService.failDataSource(
        dataSourceId,
        error.message
      );
    },
  },
  { event: DomainEvent.DATASOURCE_INITIALIZED },
  async ({ event, step }) => {
    const { dataSourceId } = event.data as DataSourceInitializedPayload;

    const dataSourceItemList = await step.run(
      "get-datasource-item-list",
      async () => {
        return await dataSourceManagementService.getDataSourceItemList(
          dataSourceId
        );
      }
    );
    await publishDataSourceItemList(dataSourceId, dataSourceItemList, step);
  }
);

export const onDataSourceRefreshRequested = inngest.createFunction(
  {
    id: "on-datasource-refresh-requested",
    onFailure: async ({ error, event }) => {
      const { dataSourceId } = event.data.event
        .data as DataSourceRefreshRequestedPayload;
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
  forRefresh: boolean = false,
  forceRefresh: boolean = false
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

export const onDataSourceItemListReceived = inngest.createFunction(
  {
    id: "on-datasource-item-list-received",
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

    await step.run("update-datasource-status", async () => {
      await dataSourceManagementService.updateDataSourceStatus(dataSourceId);
    });

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

export const onKnowledgeInitialized = inngest.createFunction(
  {
    id: "on-knowledge-initialized",
    onFailure: async ({ error, event }) => {
      const { dataSourceId, knowledgeId } = event.data.event
        .data as KnowledgeInitializedEventPayload;
      console.error(
        `Failed to initialize knowledge ${knowledgeId} for data source ${dataSourceId}`,
        error
      );
      await dataSourceManagementService.failDataSourceKnowledge(
        dataSourceId,
        knowledgeId,
        error.message
      );
    },
  },
  { event: DomainEvent.KNOWLEDGE_INITIALIZED },
  async ({ event, step }) => {
    const payload = event.data as KnowledgeInitializedEventPayload;
    const { dataSourceId, knowledgeId } = payload;

    const knowledgeWithContent = await step.run(
      "retrieve-knowledge-content",
      async () => {
        return await dataSourceManagementService.retrieveKnowledgeContent(
          dataSourceId,
          knowledgeId
        );
      }
    );

    const { indexStatus, originalContent } = knowledgeWithContent;
    if (
      indexStatus === KnowledgeIndexStatus.CONTENT_RETRIEVED &&
      originalContent
    ) {
      const eventPayload: KnowledgeContentRetrievedPayload = {
        dataSourceId,
        knowledgeId,
        originalContent,
      };
      await step.sendEvent("knowledge-content-received-event", {
        name: DomainEvent.KNOWLEDGE_CONTENT_RETRIEVED,
        data: eventPayload,
      });
    }
  }
);

export const onKnowledgeContentRetrieved = inngest.createFunction(
  {
    id: "on-knowledge-content-retrieved",
    onFailure: async ({ error, event }) => {
      const { dataSourceId, knowledgeId } = event.data.event
        .data as KnowledgeContentRetrievedPayload;
      console.error(
        `Failed to handle knowledge content retrieved for knowledge ${knowledgeId} and data source ${dataSourceId}`,
        error
      );
      await dataSourceManagementService.failDataSourceKnowledge(
        dataSourceId,
        knowledgeId,
        error.message
      );
    },
  },
  { event: DomainEvent.KNOWLEDGE_CONTENT_RETRIEVED },
  async ({ event, step }) => {
    const payload = event.data as KnowledgeContentRetrievedPayload;
    const { dataSourceId, knowledgeId, originalContent } = payload;

    await step.run("store-knowledge-content", async () => {
      return await dataSourceManagementService.storeKnowledgeContent(
        knowledgeId,
        originalContent
      );
    });

    const knowledgeWithDocuments = await step.run(
      "create-documents-from-content",
      async () => {
        return await dataSourceManagementService.createDocumentsFromContent(
          knowledgeId
        );
      }
    );

    const hasSufficientDataStorage = await step.run(
      "validate-storage-usage",
      async () => {
        return await dataSourceManagementService.validateDataStorageUsage(
          dataSourceId,
          knowledgeWithDocuments
        );
      }
    );

    if (!hasSufficientDataStorage) {
      return;
    }

    const { knowledge, knowledgeChunkEvents } = await step.run(
      "publish-knowledge-chunk-events",
      async () => {
        return await dataSourceManagementService.publishKnowledgeChunkEvents(
          dataSourceId,
          knowledgeId
        );
      }
    );

    if (knowledgeChunkEvents.length === 0) {
      await onKnowledgeStatusUpdated(dataSourceId, knowledge, step);
      return;
    }

    await step.run("persist-knowledge-chunk-events", async () => {
      return await dataSourceManagementService.persistKnowledgeChunkEvents(
        knowledgeId,
        knowledgeChunkEvents
      );
    });
  }
);

export const onKnowledgeChunkReceived = inngest.createFunction(
  {
    id: "on-knowledge-chunk-received",
    concurrency: {
      limit: 3,
    },
    onFailure: async ({ error, event }) => {
      const { dataSourceId, knowledgeId, chunkNumber } = event.data.event
        .data as KnowledgeChunkReceivedPayload;
      const errorMessage = event.data.error.message;

      console.error(
        `Failed to load chunk ${chunkNumber} of knowledge ${knowledgeId}: ${errorMessage}`
      );

      await dataSourceManagementService.failDataSourceKnowledgeChunk(
        dataSourceId,
        knowledgeId,
        chunkNumber,
        errorMessage
      );
    },
  },
  { event: DomainEvent.KNOWLEDGE_CHUNK_RECEIVED },
  async ({ event, step }) => {
    const { dataSourceId, knowledgeId, chunk, chunkNumber } =
      event.data as KnowledgeChunkReceivedPayload;

    const docIds = await step.run("load-knowledge-chunk", async () => {
      return await dataSourceManagementService.loadKnowledgeChunk(
        chunk,
        chunkNumber
      );
    });

    const chunkLoadingResult: ChunkLoadingResult = {
      docIds,
      chunkNumber,
      status: KnowledgeChunkStatus.COMPLETED,
    };

    await step.run("persist-indexing-result", async () => {
      return await dataSourceManagementService.persistChunkLoadingResult(
        knowledgeId,
        chunkLoadingResult
      );
    });

    const updatedKnowledge = await step.run(
      "update-knowledge-status",
      async () => {
        return await dataSourceManagementService.updateKnowledgeStatus(
          knowledgeId
        );
      }
    );

    await onKnowledgeStatusUpdated(dataSourceId, updatedKnowledge, step);
  }
);

const onKnowledgeStatusUpdated = async (
  dataSourceId: string,
  knowledge: KnowledgeDto,
  step: any
) => {
  if (knowledgeEndStatuses.includes(knowledge.indexStatus)) {
    const eventPayload: KnowledgeIndexingCompletedPayload = {
      dataSourceId,
      knowledge,
    };
    await step.sendEvent("knowledge-indexing-completed", {
      name: DomainEvent.KNOWLEDGE_INDEXING_COMPLETED,
      data: eventPayload,
    });
  } else {
    await step.run("update-datasource-status", async () => {
      await dataSourceManagementService.updateDataSourceStatus(dataSourceId);
    });
  }
};

export const onKnowledgeIndexingCompleted = inngest.createFunction(
  {
    id: "on-knowledge-indexing-completed",
  },
  { event: DomainEvent.KNOWLEDGE_INDEXING_COMPLETED },
  async ({ event, step }) => {
    const payload = event.data as KnowledgeIndexingCompletedPayload;
    const { dataSourceId, knowledge } = payload;

    if (knowledge.indexStatus === KnowledgeIndexStatus.COMPLETED) {
      const relatedKnowledgeIds = await step.run(
        "delete-related-knowledge-instances",
        async () => {
          return await dataSourceManagementService.deleteRelatedKnowledgeInstances(
            knowledge.id
          );
        }
      );

      await Promise.all(
        relatedKnowledgeIds.map((knowledgeId) =>
          step.run("delete-vectordb-knowledge", async () => {
            await vectorDatabaseAdapter.deleteKnowledge(knowledgeId);
          })
        )
      );
    }

    await step.run("update-datasource-status", async () => {
      await dataSourceManagementService.updateDataSourceStatus(dataSourceId);
    });
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

export const onDataSourceDeleteRequested = inngest.createFunction(
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
  { cron: "0 * * * *" },
  async ({ step }) => {
    const deletedKnowledgeIds = await step.run(
      "delete-unused-knowledges",
      async () => {
        return await dataSourceManagementService.deleteUnusedKnowledges();
      }
    );

    await Promise.all(
      deletedKnowledgeIds.flatMap((knowledgeId) => [
        step.run("delete-vectordb-knowledge", async () => {
          await vectorDatabaseAdapter.deleteKnowledge(knowledgeId);
        }),
        step.run("delete-blob-storage", async () => {
          await dataSourceManagementService.deleteBlobStorage(knowledgeId);
        }),
      ])
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
