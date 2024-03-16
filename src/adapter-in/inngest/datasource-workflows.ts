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
  KnowledgeDeletedPayload,
  KnowledgeIndexingCompletedSuccessfullyPayload,
  KnowledgeInitializedEventPayload,
  KnowledgeRetryRequestedPayload,
} from "@/src/domain/events/domain-event";
import { KnowledgeDto } from "@/src/domain/models/DataSources";
import dataSourceManagementService from "@/src/domain/services/DataSourceManagementService";
import dataSourceViewingService from "@/src/domain/services/DataSourceViewingService";
import knowledgeService from "@/src/domain/services/KnowledgeService";
import { KnowledgeChunkStatus, KnowledgeIndexStatus } from "@prisma/client";
import { inngest } from "./client";

const MAX_EVENTS_PER_STEP = 1000;

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

    if (dataSourceItemList.rootItemMissing) {
      await step.run("mark-data-source-missing", async () => {
        return await dataSourceManagementService.markDataSourceAsMissing(
          dataSourceId
        );
      });
      return;
    }

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

    const { knowledgeListToUpdate, knowledgeIdsToAssociate } = await step.run(
      "upsert-knowledge-list",
      async () => {
        return await dataSourceManagementService.upsertKnowledgeList(
          dataSourceId,
          dataSourceItemList,
          forceRefresh
        );
      }
    );

    await step.run("update-datasource-knowledge-associations", async () => {
      await dataSourceManagementService.updateDataSourceKnowledgeAssociations(
        dataSourceId,
        dataSourceItemList,
        knowledgeIdsToAssociate
      );
    });

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

    const { indexStatus, originalContent, metadata } = knowledgeWithContent;

    if (indexStatus === KnowledgeIndexStatus.FAILED) {
      await dataSourceManagementService.failDataSourceKnowledge(
        dataSourceId,
        knowledgeId,
        metadata?.errors?.knowledge ||
          `Failed to retrieve knowledge content ${knowledgeId} for data source ${dataSourceId}`
      );
      return;
    }

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

    const { knowledge, knowledgeChunkIndexes } = await step.run(
      "create-knowledge-chunks",
      async () => {
        return await dataSourceManagementService.createKnowledgeChunks(
          knowledgeId
        );
      }
    );

    if (knowledgeChunkIndexes.length === 0) {
      await onKnowledgeStatusUpdated(dataSourceId, knowledge, step);
      return;
    }

    await step.run("persist-knowledge-chunks", async () => {
      return await dataSourceManagementService.persistKnowledgeChunks(
        knowledgeId,
        knowledgeChunkIndexes
      );
    });

    let knowledgeChunkEvents = [];
    for (const chunk of knowledgeChunkIndexes) {
      const eventPayload: KnowledgeChunkReceivedPayload = {
        dataSourceId,
        knowledgeId,
        chunkNumber: chunk.chunkNumber,
      };
      knowledgeChunkEvents.push({
        name: DomainEvent.KNOWLEDGE_CHUNK_RECEIVED,
        data: eventPayload,
      });

      if (knowledgeChunkEvents.length >= MAX_EVENTS_PER_STEP) {
        await step.sendEvent(
          "fan-out-knowledge-chunk-received",
          knowledgeChunkEvents
        );
        knowledgeChunkEvents = [];
      }
    }

    if (knowledgeChunkEvents.length > 0) {
      await step.sendEvent(
        "fan-out-knowledge-chunk-received",
        knowledgeChunkEvents
      );
    }
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
    const { dataSourceId, knowledgeId, chunkNumber } =
      event.data as KnowledgeChunkReceivedPayload;

    const docIds = await step.run("load-knowledge-chunk", async () => {
      return await dataSourceManagementService.loadKnowledgeChunk(
        knowledgeId,
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

export const onKnowledgeIndexingCompletedSuccessfully = inngest.createFunction(
  {
    id: "on-knowledge-indexing-completed-successfully",
  },
  { event: DomainEvent.KNOWLEDGE_INDEXING_COMPLETED_SUCCESSFULLY },
  async ({ event, step }) => {
    const payload = event.data as KnowledgeIndexingCompletedSuccessfullyPayload;
    const { knowledgeId } = payload;

    const { deletedKnowledgeIds, updatedDataSourceIds } = await step.run(
      "delete-related-knowledge-instances",
      async () => {
        return await dataSourceManagementService.deleteRelatedKnowledgeInstances(
          knowledgeId
        );
      }
    );

    await publishKnowledgeDeletedEvents(deletedKnowledgeIds, true, true, step);

    await Promise.all(
      updatedDataSourceIds.map((dataSourceId) =>
        step.run("update-datasource-status", async () => {
          await dataSourceManagementService.updateDataSourceStatus(
            dataSourceId
          );
        })
      )
    );
  }
);

export const refreshDataSources = inngest.createFunction(
  { id: "refresh-datasources", retries: 0 },
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

    await publishKnowledgeDeletedEvents(deletedKnowledgeIds, true, true, step);
  }
);

export const onKnowledgeDeleted = inngest.createFunction(
  { id: "on-knowledge-deleted" },
  { event: DomainEvent.KNOWLEDGE_DELETED },
  async ({ event, step }) => {
    const { knowledgeId, deleteBlobStorage, deleteVectorDBStorage } =
      event.data as KnowledgeDeletedPayload;

    if (deleteBlobStorage) {
      await step.run("delete-blob-storage", async () => {
        await dataSourceManagementService.deleteBlobStorage(knowledgeId);
      });
    }

    if (deleteVectorDBStorage) {
      await deleteKnowledgeVectorStorage(knowledgeId, step);
    }
  }
);

export const deleteUnusedKnowledges = inngest.createFunction(
  { id: "delete-unused-knowledges", retries: 0 },
  { cron: "0 0 * * *" },
  async ({ step }) => {
    const deletedKnowledgeIds = await step.run(
      "delete-unused-knowledges",
      async () => {
        return await dataSourceManagementService.deleteUnusedKnowledges();
      }
    );

    await publishKnowledgeDeletedEvents(deletedKnowledgeIds, true, true, step);
  }
);

export const deleteBlobStorage = inngest.createFunction(
  { id: "delete-blob-storage", retries: 0 },
  { cron: "0 0 * * *" },
  async ({ step }) => {
    const knowledgeIds = await step.run(
      "find-deleted-knowledge-with-blob-storage",
      async () => {
        return await knowledgeService.findDeletedKnowledgeWithBlobStorage();
      }
    );

    await Promise.all(
      knowledgeIds.map((knowledgeId) =>
        step.run("delete-blob-storage", async () => {
          await dataSourceManagementService.deleteBlobStorage(knowledgeId);
        })
      )
    );
  }
);

export const deleteVectorDBStorage = inngest.createFunction(
  { id: "delete-vectordb-storage", retries: 0 },
  { cron: "0 0 * * *" },
  async ({ step }) => {
    const numberOfIdsToFetch = 1000;
    const knowledgeIds = await step.run(
      "find-deleted-knowledge-with-vector-storage",
      async () => {
        return await knowledgeService.findDeletedKnowledgeWithVectorStorage(
          numberOfIdsToFetch
        );
      }
    );

    await publishKnowledgeDeletedEvents(knowledgeIds, false, true, step);
  }
);

export const deleteRelatedKnowledgeInstances = inngest.createFunction(
  { id: "delete-related-knowledge-instances" },
  { cron: "0 0 * * *" },
  async ({ step }) => {
    const knowledgeIds = await step.run(
      "find-knowledge-with-related-knowledge-instances",
      async () => {
        return await knowledgeService.findCompletedKnowledgeWithRelatedInstances();
      }
    );

    let events: {
      name: string;
      data: KnowledgeIndexingCompletedSuccessfullyPayload;
    }[] = [];
    for (const knowledgeId of knowledgeIds) {
      const eventPayload: KnowledgeIndexingCompletedSuccessfullyPayload = {
        knowledgeId,
      };
      events.push({
        name: DomainEvent.KNOWLEDGE_INDEXING_COMPLETED_SUCCESSFULLY,
        data: eventPayload,
      });

      if (events.length >= MAX_EVENTS_PER_STEP) {
        await step.sendEvent(
          "knowledge-indexing-completed-successfully",
          events
        );
        events = [];
      }
    }

    if (events.length > 0) {
      await step.sendEvent("knowledge-indexing-completed-successfully", events);
    }
  }
);

export const retryFailedKnowledge = inngest.createFunction(
  { id: "retry-failed-knowledge" },
  { cron: "0 0 * * *" },
  async ({ step }) => {
    const batchSize = 100;
    const knowledgeIds = await step.run("find-failed-knowledge", async () => {
      return await knowledgeService.findFailedKnowledge(batchSize);
    });

    await Promise.all(
      knowledgeIds.map((knowledgeId) => {
        const knowledgeRetryRequestedPayload: KnowledgeRetryRequestedPayload = {
          knowledgeId,
        };
        return step.sendEvent("knowledge-deleted", {
          name: DomainEvent.KNOWLEDGE_RETRY_REQUESTED,
          data: knowledgeRetryRequestedPayload,
        });
      })
    );
  }
);

export const onKnowledgeRetryRequested = inngest.createFunction(
  {
    id: "on-knowledge-retry-requested",
  },
  { event: DomainEvent.KNOWLEDGE_RETRY_REQUESTED },
  async ({ event, step }) => {
    const { knowledgeId } = event.data as KnowledgeRetryRequestedPayload;

    const { vectorIds } = await step.run("vector-id-list", async () => {
      return await vectorDatabaseAdapter.vectorIdList(knowledgeId);
    });

    if (vectorIds.length > 0) {
      // Delete any existing vectors first before retrying
      // Return and expect another event to be published for retry, as vector deletion is asynchronous
      await deleteKnowledgeVectorStorage(knowledgeId, step);
      return;
    }

    const resetKnowledge = await step.run(
      "reset-knowledge-chunks",
      async () => {
        return await knowledgeService.resetKnowledgeChunks(knowledgeId);
      }
    );

    const dataSourceId = await step.run("get-datasource-id", async () => {
      return await dataSourceViewingService.getOriginalDataSourceIdForKnowledge(
        knowledgeId
      );
    });

    if (!dataSourceId) {
      // No data source associated with knowledge, knowledge is orphaned
      // Knowledge will be deleted by deleteUnusedKnowledges
      return;
    }

    if (resetKnowledge.originalContent) {
      const eventPayload: KnowledgeContentRetrievedPayload = {
        dataSourceId,
        knowledgeId,
        originalContent: resetKnowledge.originalContent,
      };
      await step.sendEvent("knowledge-content-received-event", {
        name: DomainEvent.KNOWLEDGE_CONTENT_RETRIEVED,
        data: eventPayload,
      });
    }
  }
);

const onKnowledgeStatusUpdated = async (
  dataSourceId: string,
  knowledge: KnowledgeDto,
  step: any
) => {
  await step.run("update-datasource-status", async () => {
    await dataSourceManagementService.updateDataSourceStatus(dataSourceId);
  });

  if (knowledge.indexStatus === KnowledgeIndexStatus.COMPLETED) {
    const eventPayload: KnowledgeIndexingCompletedSuccessfullyPayload = {
      knowledgeId: knowledge.id,
    };
    await step.sendEvent("knowledge-indexing-completed-successfully", {
      name: DomainEvent.KNOWLEDGE_INDEXING_COMPLETED_SUCCESSFULLY,
      data: eventPayload,
    });
  }
};

const publishKnowledgeDeletedEvents = async (
  deletedKnowledgeIds: string[],
  deleteBlobStorage: boolean,
  deleteVectorDBStorage: boolean,
  step: any
) => {
  await Promise.all(
    deletedKnowledgeIds.map((knowledgeId) => {
      const knowledgeDeletedPayload: KnowledgeDeletedPayload = {
        knowledgeId,
        deleteBlobStorage,
        deleteVectorDBStorage,
      };
      return step.sendEvent("knowledge-deleted", {
        name: DomainEvent.KNOWLEDGE_DELETED,
        data: knowledgeDeletedPayload,
      });
    })
  );
};

const deleteKnowledgeVectorStorage = async (knowledgeId: string, step: any) => {
  let paginationNextToken: string;
  let vectorIdsToDelete: string[] = [];
  let count = 0;
  do {
    const { vectorIds, paginationNextToken: newPaginationNextToken } =
      await step.run("vector-id-list", async () => {
        return await vectorDatabaseAdapter.vectorIdList(
          knowledgeId,
          paginationNextToken
        );
      });
    paginationNextToken = newPaginationNextToken;
    vectorIdsToDelete.push(...vectorIds);
    count++;

    // Loop while paginationNextToken is not null, indicating that there are more vectors to delete
    // Limit to 100 loops to avoid running into timeouts or hitting inngest limit for maximum steps in a workflow
  } while (paginationNextToken && count < 100);

  if (vectorIdsToDelete.length > 0) {
    await step.run("delete-vectors", async () => {
      const batchSize = 1000;
      for (let i = 0; i < vectorIdsToDelete.length; i += batchSize) {
        let batch = vectorIdsToDelete.slice(i, i + batchSize);
        await vectorDatabaseAdapter.deleteVectors(batch);
      }
    });
  }

  if (paginationNextToken) {
    // Publish another KnowledgeDeleted event to continue deleting vectors with a new function invocation
    await publishKnowledgeDeletedEvents([knowledgeId], false, true, step);
  } else {
    await step.run("set-vector-storage-as-deleted", async () => {
      await knowledgeService.setVectorStorageAsDeleted(knowledgeId);
    });
  }
};
