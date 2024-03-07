import { UpdateDataSourceRequest } from "@/src/adapter-in/api/DataSourcesApi";
import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import fileLoader from "@/src/adapter-out/knowledge/knowledgeLoaders/FileLoader";
import {
  DataSourceItemList,
  KnowledgeOriginalContent,
  RetrieveContentResponseStatus,
} from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import {
  ChunkLoadingResult,
  KnowledgeChunkEvent,
  KnowledgeChunkIndexes,
} from "@/src/adapter-out/knowledge/types/KnowledgeChunkTypes";
import { DataSourceRepositoryImpl } from "@/src/adapter-out/repositories/DataSourceRepositoryImpl";
import { KnowledgeRepositoryImpl } from "@/src/adapter-out/repositories/KnowledgeRepositoryImpl";
import prismadb from "@/src/lib/prismadb";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { DataSourceSecurityService } from "@/src/security/services/DataSourceSecurityService";
import { Document } from "@langchain/core/documents";
import {
  DataSource,
  DataSourceIndexStatus,
  DataSourceRefreshPeriod,
  DataSourceType,
  Knowledge,
  KnowledgeChunkStatus,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import {
  DataSourceInitializedPayload,
  DataSourceRefreshRequestedPayload,
  DomainEvent,
} from "../events/domain-event";
import { DataSourceDto, KnowledgeDto } from "../models/DataSources";
import { DataSourceRepository } from "../ports/outgoing/DataSourceRepository";
import { KnowledgeRepository } from "../ports/outgoing/KnowledgeRepository";
import dataSourceAdapterService from "./DataSourceAdapterService";
import { FileStorageService } from "./FileStorageService";
import usageService from "./UsageService";

const KNOWLEDGE_CHUNK_TOKEN_COUNT = 20000;

export class DataSourceManagementService {
  constructor(
    private dataSourceRepository: DataSourceRepository,
    private knowledgeRepository: KnowledgeRepository
  ) {}

  /**
   * Persist a new data source entity, in INITIALIZED status
   * Publishes a DATASOURCE_INITIALIZED event.
   * @param authorizationContext
   * @param name
   * @param type
   * @param data
   * @returns
   */
  public async initializeDataSource(
    authorizationContext: AuthorizationContext,
    name: string,
    type: DataSourceType,
    refreshPeriod: DataSourceRefreshPeriod,
    data: any
  ) {
    const { orgId, userId } = authorizationContext;

    const dataSource = await this.dataSourceRepository.initializeDataSource(
      orgId,
      userId,
      name,
      type,
      refreshPeriod,
      data
    );

    const dataSourceId = dataSource.id;
    const eventPayload: DataSourceInitializedPayload = {
      dataSourceId,
    };
    await publishEvent(DomainEvent.DATASOURCE_INITIALIZED, eventPayload);

    return dataSourceId;
  }

  /**
   * Retrieves and persists knowledge list for the specified data source
   * @param dataSourceId
   * @returns
   */
  public async getDataSourceItemList(
    dataSourceId: string,
    forRefresh: boolean = false,
    forceRefresh: boolean = false
  ) {
    const { dataSource, dataSourceAdapter } =
      await dataSourceAdapterService.getDataSourceAndAdapter(dataSourceId);

    return await dataSourceAdapter.getDataSourceItemList(
      dataSource.orgId,
      dataSource.ownerUserId,
      dataSourceId,
      dataSource.data,
      forRefresh,
      forceRefresh
    );
  }

  /**
   * Upserts a knowledge list for the specified data source to match the
   * provided list of items. For each item in the list, the upsert logic works as follows:
   * - A new knowledge is created in INITIALIZED status if:
   *   - A knowledge with the same uniqueId does not exist
   *   - A knowledge with the same uniqueId exists but should be reindexed (based on the adapter's shouldReindexKnowledge method)
   * - A new knowledge is not created if a knowledge with the same uniqueId exists and should not be reindexed
   *
   * The method returns a list of all knowledges, including both new and existing knowledges.
   * @param dataSourceId
   * @param itemList
   * @returns
   */
  public async upsertKnowledgeList(
    dataSourceId: string,
    itemList: DataSourceItemList,
    forceRefresh: boolean
  ): Promise<KnowledgeDto[]> {
    if (itemList.items.length === 0) {
      return [];
    }
    const { dataSource, dataSourceAdapter } =
      await dataSourceAdapterService.getDataSourceAndAdapter(dataSourceId);

    const knowledgeList = [];
    const existingKnowledgeMap = await this.getExistingKnowledgeMap(
      dataSource,
      itemList
    );

    for (const item of itemList.items) {
      let knowledge = existingKnowledgeMap.get(item.uniqueId!);

      if (
        knowledge &&
        (forceRefresh ||
          dataSourceAdapter.shouldReindexKnowledge(knowledge, item))
      ) {
        knowledge = undefined;
      }

      if (!knowledge) {
        knowledge = await prismadb.knowledge.create({
          data: {
            name: item.name,
            type: dataSource.type,
            uniqueId: item.uniqueId,
            indexStatus: KnowledgeIndexStatus.INITIALIZED,
            originalContent: item.originalContent as any,
            metadata: item.metadata,
            isMigrated: true,
          },
        });
      }

      knowledgeList.push(knowledge);
    }

    return knowledgeList.map((knowledge) => this.mapKnowledgeToDto(knowledge));
  }

  private mapKnowledgeToDto(knowledge: Knowledge): KnowledgeDto {
    const {
      id,
      name,
      type,
      uniqueId,
      indexStatus,
      documentCount,
      tokenCount,
      originalContent,
      documentsBlobUrl,
      metadata,
      indexPercentage,
      isBlobStorageDeleted,
      ...rest
    } = knowledge;

    return {
      id,
      name,
      type,
      uniqueId,
      indexStatus,
      documentCount,
      tokenCount,
      originalContent: originalContent as unknown as KnowledgeOriginalContent,
      documentsBlobUrl,
      indexPercentage: indexPercentage.toString(),
      metadata,
      isBlobStorageDeleted,
    };
  }

  private async getExistingKnowledgeMap(
    dataSource: DataSource,
    itemList: DataSourceItemList
  ) {
    const existingKnowledgeMap = new Map<string, Knowledge>();
    const uniqueIds = itemList.items
      .map((item) => item.uniqueId)
      .filter((uniqueId): uniqueId is string => uniqueId !== undefined);

    if (uniqueIds.length > 0) {
      const existingKnowledge = await prismadb.knowledge.findMany({
        where: {
          type: dataSource.type,
          uniqueId: { in: uniqueIds },
          indexStatus: KnowledgeIndexStatus.COMPLETED,
        },
      });

      existingKnowledge.forEach((knowledge) => {
        existingKnowledgeMap.set(knowledge.uniqueId!, knowledge);
      });
    }

    return existingKnowledgeMap;
  }

  /**
   * Creates associations between the specified data source and the specified knowledge list
   * @param dataSourceId
   * @param knowledgeIds
   */
  public async createDataSourceKnowledgeAssociations(
    dataSourceId: string,
    knowledgeIds: string[]
  ) {
    if (knowledgeIds.length === 0) {
      return;
    }

    const dataSourceKnowledgeRelations = knowledgeIds.map((knowledgeId) => {
      return { dataSourceId, knowledgeId };
    });

    await prismadb.dataSourceKnowledge.createMany({
      data: dataSourceKnowledgeRelations,
    });
  }

  /**
   *
   * @param dataSource
   * @param dataSourceAdapter
   * @param itemList
   * @param knowledgeList
   */
  public async updateDataSourceKnowledgeAssociations(
    dataSourceId: string,
    itemList: DataSourceItemList,
    knowledgeList: KnowledgeDto[]
  ) {
    const { dataSourceAdapter } =
      await dataSourceAdapterService.getDataSourceAndAdapter(dataSourceId);
    const knowledgeIds = knowledgeList.map((knowledge) => knowledge.id);

    // Find existing associations
    const existingAssociations = await prismadb.dataSourceKnowledge.findMany({
      where: {
        dataSourceId,
        knowledgeId: {
          in: knowledgeIds,
        },
      },
      select: {
        knowledgeId: true,
      },
    });
    const existingKnowledgeIds = existingAssociations.map(
      (association) => association.knowledgeId
    );

    // Filter to include only new relationships
    const newKnowledgeIds = knowledgeIds.filter(
      (knowledgeId) => !existingKnowledgeIds.includes(knowledgeId)
    );

    if (newKnowledgeIds.length > 0) {
      const newDataSourceKnowledgeRelations = newKnowledgeIds.map(
        (knowledgeId) => {
          return { dataSourceId, knowledgeId };
        }
      );

      await prismadb.dataSourceKnowledge.createMany({
        data: newDataSourceKnowledgeRelations,
      });
    }

    // Remove relationships to any knowledge IDs which have been removed from the data source
    const removedKnowledgeIds = await dataSourceAdapter.getRemovedKnowledgeIds(
      itemList
    );
    if (removedKnowledgeIds.length > 0) {
      await prismadb.dataSourceKnowledge.deleteMany({
        where: {
          dataSourceId,
          knowledgeId: {
            in: removedKnowledgeIds,
          },
        },
      });
    }
  }

  /**
   * Retrieves knowledge content for the specified data source and knowledge
   * We have 3 possible scenarios:
   * 1. The knowledge content has already been retrieved and is available in the contentBlobUrl field
   * 2. The knowledge content is retrieve synchronously by the data source adapter and a contentBlobUrl is returned
   * 3. The knowledge content is retrieved asynchronously by the data source adapter. In this case, the knowledge
   * is updated with a status of RETRIEVING_CONTENT and the contentBlobUrl field is left empty.
   *
   * For cases 1 & 2, the knowledge is updated with the contentBlobUrl and a status of CONTENT_RETRIEVED
   * @param dataSourceId
   * @param knowledgeId
   */
  public async retrieveKnowledgeContent(
    dataSourceId: string,
    knowledgeId: string
  ): Promise<KnowledgeDto> {
    const dataSource = await this.dataSourceRepository.getById(dataSourceId);
    const knowledge = await this.knowledgeRepository.getById(knowledgeId);

    let originalContent: KnowledgeOriginalContent | undefined =
      knowledge.originalContent as unknown as KnowledgeOriginalContent;
    let knowledgeStatus: KnowledgeIndexStatus =
      KnowledgeIndexStatus.CONTENT_RETRIEVED;
    let newMetadata: any;

    if (!originalContent) {
      const dataSourceAdapter =
        dataSourceAdapterService.getContentRetrievingDataSourceAdapter(
          knowledge.type
        );

      const result = await dataSourceAdapter.retrieveKnowledgeContent(
        dataSource.orgId,
        dataSource.ownerUserId,
        dataSourceId,
        knowledge,
        dataSource.data
      );

      const statusMapping = {
        [RetrieveContentResponseStatus.PENDING]:
          KnowledgeIndexStatus.RETRIEVING_CONTENT,
        [RetrieveContentResponseStatus.SUCCESS]:
          KnowledgeIndexStatus.CONTENT_RETRIEVED,
        [RetrieveContentResponseStatus.FAILED]: KnowledgeIndexStatus.FAILED,
      };

      knowledgeStatus = statusMapping[result.status];
      originalContent = result.originalContent;
      newMetadata = result.metadata;
    }

    // Merge existing metadata with the new metadata
    const updatedMetadata = this.mergeMetadata(knowledge.metadata, newMetadata);
    return await this.knowledgeRepository.update(knowledgeId, {
      indexStatus: knowledgeStatus,
      metadata: updatedMetadata,
      originalContent: originalContent as any,
    });
  }

  /**
   * Stores a reference to the knowledge content in the knowledge entity,
   * in the originalContent field
   * @param knowledgeId
   * @param originalContent
   * @returns
   */
  public async storeKnowledgeContent(
    knowledgeId: string,
    originalContent: KnowledgeOriginalContent
  ) {
    return await this.knowledgeRepository.update(knowledgeId, {
      indexStatus: KnowledgeIndexStatus.CONTENT_RETRIEVED,
      originalContent: originalContent as any,
    });
  }

  /**
   * Retrieves knowledge content for the specified knowledge and
   * computes the documents and metadata using the appropriate langchain loader
   * The computed documents are uploaded to the file storage service
   *
   * @param knowledgeId
   */
  public async createDocumentsFromContent(
    knowledgeId: string
  ): Promise<KnowledgeDto> {
    const knowledge = await this.knowledgeRepository.getById(knowledgeId);
    const { originalContent, documentsBlobUrl: existingDocumentsBlobUrl } =
      knowledge;

    if (existingDocumentsBlobUrl) {
      // Documents have already been created
      return knowledge;
    }

    if (!originalContent) {
      throw new Error(
        `Knowledge with id=${knowledgeId} does not have original content`
      );
    }

    const { filename, mimeType, contentBlobUrl } = originalContent;
    const contentBlob = await FileStorageService.getBlob(contentBlobUrl);

    const { docs, metadata } = await fileLoader.getLangchainDocs(
      knowledgeId,
      filename,
      mimeType,
      contentBlob
    );

    const documentsBlobUrl = await FileStorageService.put(
      `${knowledge.name}.json`,
      JSON.stringify(docs)
    );

    return await this.knowledgeRepository.update(knowledgeId, {
      indexStatus: KnowledgeIndexStatus.DOCUMENTS_CREATED,
      documentsBlobUrl,
      documentCount: metadata.documentCount,
      tokenCount: metadata.totalTokenCount,
    });
  }

  /**
   * Validates that the organization associated with the data source has sufficient data storage.
   * If the organization does not have sufficient data storage, the data source is updated with a status of FAILED
   * @param dataSourceId
   * @param knowledge
   * @returns
   */
  public async validateDataStorageUsage(
    dataSourceId: string,
    knowledge: KnowledgeDto
  ): Promise<boolean> {
    if (!knowledge.tokenCount) {
      return true;
    }

    const dataSource = await this.dataSourceRepository.getById(dataSourceId);

    const hasSufficientDataStorage =
      await usageService.hasSufficientDataStorage(
        dataSource.orgId,
        knowledge.tokenCount
      );

    if (!hasSufficientDataStorage) {
      await this.dataSourceRepository.updateDataSource(dataSourceId, {
        indexStatus: DataSourceIndexStatus.FAILED,
      });
    }

    return hasSufficientDataStorage;
  }

  /**
   * Retrieves knowledge content for the specified knowledge from file storage and breaks down content
   * into chunks. Each document batch holds up to a maximum of KNOWLEDGE_CHUNK_TOKEN_COUNT tokens.
   * Returns a list of knowledge chunk indexes, which hold the start & end index of each batch
   * @param knowledgeId
   */
  public async createKnowledgeChunks(knowledgeId: string): Promise<{
    knowledge: KnowledgeDto;
    knowledgeChunkIndexes: KnowledgeChunkIndexes[];
  }> {
    const knowledge = await this.knowledgeRepository.getById(knowledgeId);

    const { documentsBlobUrl } = knowledge;
    if (!documentsBlobUrl) {
      throw new Error(
        `Knowledge with id=${knowledgeId} does not have documents blob url`
      );
    }

    const documentsJson = await FileStorageService.getJson(documentsBlobUrl);
    const documents: Document[] = documentsJson;

    let chunkIndexes: KnowledgeChunkIndexes[] =
      this.getKnowledgeChunkIndexes(documents);
    const chunkCount = chunkIndexes.length;

    const updatedMetadata = this.mergeMetadata(knowledge.metadata, {
      chunkCount,
    });

    let indexStatus;
    let indexPercentage;
    if (chunkCount === 0) {
      indexStatus = KnowledgeIndexStatus.COMPLETED;
      indexPercentage = 100;
    } else {
      indexStatus = KnowledgeIndexStatus.INDEXING;
    }
    const updatedKnowledge = await this.knowledgeRepository.update(
      knowledgeId,
      {
        indexStatus,
        indexPercentage,
        metadata: updatedMetadata,
      }
    );

    return { knowledge: updatedKnowledge, knowledgeChunkIndexes: chunkIndexes };
  }

  private getKnowledgeChunkIndexes(documents: Document[]) {
    let chunks: KnowledgeChunkIndexes[] = [];
    let currentChunkStartIndex = 0;
    let currentTokenCount = 0;

    for (let i = 0; i < documents.length; i++) {
      const document = documents[i];

      // Check if adding the current document exceeds the limit
      // And if currentTokenCount is not 0 (the batch is not empty)
      if (
        currentTokenCount + document.metadata.tokenCount >
          KNOWLEDGE_CHUNK_TOKEN_COUNT &&
        currentTokenCount !== 0
      ) {
        // Start a new batch
        chunks.push({
          chunkNumber: chunks.length,
          startIndex: currentChunkStartIndex,
          endIndex: i - 1,
        });
        currentChunkStartIndex = i;
        currentTokenCount = 0;
      }

      currentTokenCount += document.metadata.tokenCount;
    }

    // Add the last batch if it has documents
    if (currentChunkStartIndex < documents.length) {
      chunks.push({
        chunkNumber: chunks.length,
        startIndex: currentChunkStartIndex,
        endIndex: documents.length - 1,
      });
    }

    return chunks;
  }

  public async persistKnowledgeChunks(
    knowledgeId: string,
    knowledgeChunkIndexes: KnowledgeChunkIndexes[]
  ) {
    await this.knowledgeRepository.persistKnowledgeChunks(
      knowledgeId,
      knowledgeChunkIndexes
    );
  }

  public async persistKnowledgeChunkEvents(
    knowledgeId: string,
    knowledgeChunkEvents: KnowledgeChunkEvent[]
  ) {
    await this.knowledgeRepository.persistKnowledgeChunkEvents(
      knowledgeId,
      knowledgeChunkEvents
    );
  }

  /**
   * Loads a chunk of knowledge content into the knowledge index
   * @param chunk
   * @param chunkNumber
   * @returns
   */
  public async loadKnowledgeChunk(knowledgeId: string, chunkNumber: number) {
    const knowledge = await this.knowledgeRepository.getById(knowledgeId);
    const { documentsBlobUrl } = knowledge;
    if (!documentsBlobUrl) {
      throw new Error(
        `Knowledge with id=${knowledgeId} does not have documents blob url`
      );
    }
    const knowledgeChunk =
      await this.knowledgeRepository.getKnowledgeChunkByNumber(
        knowledgeId,
        chunkNumber
      );

    const documentsJson = await FileStorageService.getJson(documentsBlobUrl);
    const documents: Document[] = documentsJson;
    const { startIndex, endIndex } = knowledgeChunk;
    if (startIndex === null || endIndex === null) {
      throw new Error(
        `Knowledge chunk with knowledgeId=${knowledgeId} and chunkNumber=${chunkNumber} does not have start or end index`
      );
    }

    const documentsToLoad = documents.slice(startIndex, endIndex + 1);
    return await fileLoader.loadDocs(documentsToLoad, chunkNumber);
  }

  /**
   * Persists the result of loading a chunk of knowledge content into the knowledge index
   * @param knowledgeId
   * @param chunkLoadingResult
   * @returns
   */
  public async persistChunkLoadingResult(
    knowledgeId: string,
    chunkLoadingResult: ChunkLoadingResult
  ): Promise<void> {
    const { chunkNumber, status, error } = chunkLoadingResult;

    await this.knowledgeRepository.updateKnowledgeChunkStatus(
      knowledgeId,
      chunkNumber,
      status,
      error
    );
  }

  /**
   * Updates the status and indexPercentage of the specified knowledge based on the status of its chunks.
   * indexPercentage is calculated at the number of completed chunks / total chunks
   * The status is set to as follows:
   * - COMPLETED if all chunks are completed
   * - FAILED if all chunks are failed,
   * - PARTIALLY_COMPLETED if all chunks have been processed but some have failed
   * - Otherwise, the status is left unchanged
   *
   * @param knowledgeId
   * @returns
   */
  public async updateKnowledgeStatus(
    knowledgeId: string
  ): Promise<KnowledgeDto> {
    const { totalCount, completedCount, failedCount } =
      await this.knowledgeRepository.getKnowledgeChunkCounts(knowledgeId);

    const indexPercentage = (completedCount / totalCount) * 100;

    let indexStatus;
    if (completedCount === totalCount) {
      indexStatus = KnowledgeIndexStatus.COMPLETED;
    } else if (failedCount === totalCount) {
      indexStatus = KnowledgeIndexStatus.FAILED;
    } else if (completedCount + failedCount === totalCount) {
      indexStatus = KnowledgeIndexStatus.PARTIALLY_COMPLETED;
    }

    const updatedKnowledge = await this.knowledgeRepository.update(
      knowledgeId,
      {
        indexPercentage,
        indexStatus,
      }
    );

    return updatedKnowledge;
  }

  public async updateDataSourceStatus(dataSourceId: string) {
    const knowledgeCounts = await this.knowledgeRepository.getKnowledgeCounts(
      dataSourceId
    );

    const {
      completedCount,
      partiallyCompletedCount,
      failedCount,
      totalCount,
      totalDocumentCount,
      totalTokenCount,
      indexPercentage,
    } = knowledgeCounts;

    let indexStatus;
    if (completedCount === totalCount) {
      indexStatus = DataSourceIndexStatus.COMPLETED;
    } else if (failedCount === totalCount) {
      indexStatus = DataSourceIndexStatus.FAILED;
    } else if (
      completedCount + failedCount + partiallyCompletedCount ===
      totalCount
    ) {
      indexStatus = DataSourceIndexStatus.PARTIALLY_COMPLETED;
    }

    let lastIndexedAt;
    if (indexStatus) {
      lastIndexedAt = new Date();
    }

    await this.dataSourceRepository.updateDataSource(dataSourceId, {
      indexStatus,
      indexPercentage,
      lastIndexedAt,
      documentCount: totalDocumentCount,
      tokenCount: totalTokenCount,
    });
  }

  /**
   *
   * @returns Returns a list of ids of DataSources which are in Indexing status
   * and haven't been updated in the past hours
   */
  public async getIndexingDataSources() {
    const currentDate = new Date();
    const oneHourAgo = new Date(currentDate.getTime() - 60 * 60 * 1000);

    return await prismadb.dataSource.findMany({
      select: {
        id: true,
        type: true,
      },
      where: {
        indexStatus: DataSourceIndexStatus.INDEXING,
        updatedAt: {
          lt: oneHourAgo,
        },
        type: DataSourceType.WEB_URL, // limit to WEB_URLs for now
      },
    });
  }

  public async pollDataSource(
    dataSourceId: string,
    dataSourceType: DataSourceType
  ) {
    // TODO: Re-implement
    // const knowledgeList = await prismadb.knowledge.findMany({
    //   where: {
    //     indexStatus: {
    //       in: [KnowledgeIndexStatus.INITIALIZED, KnowledgeIndexStatus.INDEXING],
    //     },
    //     dataSources: {
    //       some: {
    //         dataSourceId: dataSourceId,
    //       },
    //     },
    //   },
    // });
    // const dataSourceAdapter =
    //   dataSourceAdapterService.getDataSourceAdapter(dataSourceType);
    // for (const knowledge of knowledgeList) {
    //   const indexKnowledgeResponse =
    //     await dataSourceAdapter.pollKnowledgeIndexingStatus(knowledge);
    // }
    // await this.updateDataSourceStatus(dataSourceId);
  }

  public async deleteRelatedKnowledgeInstances(knowledgeId: string): Promise<{
    deletedKnowledgeIds: string[];
    updatedDataSourceIds: string[];
  }> {
    const response: {
      deletedKnowledgeIds: string[];
      updatedDataSourceIds: string[];
    } = {
      deletedKnowledgeIds: [],
      updatedDataSourceIds: [],
    };
    const knowledge = await knowledgeRepository.getById(knowledgeId);

    if (
      !knowledge.uniqueId ||
      knowledge.indexStatus !== KnowledgeIndexStatus.COMPLETED
    ) {
      return response;
    }

    const relatedKnowledgeInstances = await prismadb.knowledge.findMany({
      where: {
        uniqueId: knowledge.uniqueId,
        type: knowledge.type,
        id: { not: knowledgeId },
      },
    });
    const relatedKnowledgeIds = relatedKnowledgeInstances.map(
      (knowledge) => knowledge.id
    );

    if (relatedKnowledgeIds.length === 0) {
      return response;
    }

    response.deletedKnowledgeIds = relatedKnowledgeIds;
    const relatedAndNewKnowledgeIds = [...relatedKnowledgeIds, knowledgeId];

    // Find all data sources which are associated with the related or the new knowledge instances
    const relatedDataSources = await prismadb.dataSourceKnowledge.findMany({
      select: { dataSourceId: true },
      distinct: ["dataSourceId"],
      where: {
        knowledgeId: { in: relatedAndNewKnowledgeIds },
      },
    });

    const newDataSourceRelationships: {
      dataSourceId: string;
      knowledgeId: string;
    }[] = [];
    for (const dataSource of relatedDataSources) {
      const dataSourceId = dataSource.dataSourceId;
      newDataSourceRelationships.push({ dataSourceId, knowledgeId });
      response.updatedDataSourceIds.push(dataSourceId);
    }

    await prismadb.$transaction(async (tx) => {
      // Delete all data source knowledge relationships for the related and new knowledge instances
      await tx.dataSourceKnowledge.deleteMany({
        where: {
          knowledgeId: { in: relatedAndNewKnowledgeIds },
        },
      });

      await tx.knowledge.updateMany({
        where: {
          id: { in: relatedKnowledgeIds },
          indexStatus: { not: KnowledgeIndexStatus.DELETED },
        },
        data: { indexStatus: KnowledgeIndexStatus.DELETED },
      });

      await tx.dataSourceKnowledge.createMany({
        data: newDataSourceRelationships,
      });
    });

    return response;
  }

  /**
   * Handles a request to delete a data source
   * Validates that the data source exists and that the user has permission to delete it
   * If validation is successful, publishes a DATASOURCE_DELETE_REQUESTED event
   * Data source deletion is handled asynchronously by the data source workflows
   * @param authorizationContext
   * @param dataSourceId
   */
  public async requestDeleteDataSource(
    authorizationContext: AuthorizationContext,
    dataSourceId: string
  ) {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
    });
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    const canUpdateDataSource = DataSourceSecurityService.canUpdateDataSource(
      authorizationContext,
      dataSource
    );

    if (!canUpdateDataSource) {
      throw new ForbiddenError("Forbidden");
    }

    await prismadb.dataSource.update({
      where: { id: dataSourceId },
      data: {
        indexStatus: DataSourceIndexStatus.DELETION_REQUESTED,
      },
    });

    await publishEvent(DomainEvent.DATASOURCE_DELETE_REQUESTED, {
      dataSourceId: dataSource.id,
    });
  }

  /**
   * Marks a data source as deleted and removes
   * all related associations.
   * Returns a list of knowledge ids which were deleted
   * @param dataSourceId
   * @returns
   */
  public async deleteDataSource(dataSourceId: string) {
    const dataSourceKnowledgeToDelete =
      await prismadb.dataSourceKnowledge.findMany({
        select: { knowledgeId: true },
        where: {
          dataSourceId,
          knowledge: {
            NOT: {
              indexStatus: {
                in: [
                  KnowledgeIndexStatus.COMPLETED,
                  KnowledgeIndexStatus.PARTIALLY_COMPLETED,
                  KnowledgeIndexStatus.DELETED,
                ],
              },
            },
          },
        },
      });

    const knowledgeIdsToDelete: string[] = dataSourceKnowledgeToDelete.map(
      (dataSourceKnowledge) => dataSourceKnowledge.knowledgeId
    );

    await prismadb.$transaction(async (tx) => {
      await tx.dataSource.update({
        where: { id: dataSourceId },
        data: {
          indexStatus: DataSourceIndexStatus.DELETED,
        },
      });

      await tx.aIDataSource.deleteMany({
        where: { dataSourceId },
      });

      await tx.dataSourceKnowledge.deleteMany({
        where: { dataSourceId },
      });

      await tx.knowledge.updateMany({
        data: {
          indexStatus: KnowledgeIndexStatus.DELETED,
        },
        where: {
          id: { in: knowledgeIdsToDelete },
        },
      });
    });

    return knowledgeIdsToDelete;
  }

  public async findDataSourcesToRefresh() {
    return await this.dataSourceRepository.findDataSourceIdsToRefresh(
      new Date()
    );
  }

  public async refreshDataSourceAsUser(
    authorizationContext: AuthorizationContext,
    dataSourceId: string,
    forceRefresh?: boolean
  ) {
    const dataSource = await this.getDataSource(dataSourceId);

    const canUpdateDataSource = DataSourceSecurityService.canUpdateDataSource(
      authorizationContext,
      dataSource
    );

    if (!canUpdateDataSource) {
      throw new ForbiddenError("Forbidden");
    }

    await this.refreshDataSourceAndPublishEvent(dataSource, forceRefresh);
  }

  public async refreshDataSourceAsSystem(
    dataSourceId: string,
    forceRefresh: boolean = false
  ) {
    const dataSource = await this.dataSourceRepository.findById(dataSourceId);
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    await this.refreshDataSourceAndPublishEvent(dataSource, forceRefresh);
  }

  private async getDataSource(dataSourceId: string) {
    const dataSource = await this.dataSourceRepository.findById(dataSourceId);
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }
    return dataSource;
  }

  private async refreshDataSourceAndPublishEvent(
    dataSource: DataSourceDto,
    forceRefresh: boolean = false
  ) {
    const dataSourceId = dataSource.id;
    await this.dataSourceRepository.updateDataSource(dataSourceId, {
      indexStatus: DataSourceIndexStatus.REFRESHING,
    });
    const eventPayload: DataSourceRefreshRequestedPayload = {
      dataSourceId,
      forceRefresh,
    };

    await publishEvent(DomainEvent.DATASOURCE_REFRESH_REQUESTED, eventPayload);
  }

  public async updateDataSource(
    authorizationContext: AuthorizationContext,
    dataSourceId: string,
    updateRequest: UpdateDataSourceRequest
  ) {
    const dataSource = await this.getDataSource(dataSourceId);

    const canUpdateDataSource = DataSourceSecurityService.canUpdateDataSource(
      authorizationContext,
      dataSource
    );

    if (!canUpdateDataSource) {
      throw new ForbiddenError("Forbidden");
    }

    if (updateRequest.refreshPeriod) {
      return await this.dataSourceRepository.updateDataSource(dataSourceId, {
        refreshPeriod: updateRequest.refreshPeriod,
      });
    }

    if (updateRequest.ais) {
      await this.dataSourceRepository.updateDataSourceAis(
        dataSourceId,
        updateRequest.ais
      );
    }
    return dataSource;
  }

  /**
   * Marks a data source as failed
   * @param dataSourceId
   * @param error
   */
  public async failDataSource(dataSourceId: string, error: string) {
    const ds = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
    });
    if (!ds) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    await prismadb.dataSource.update({
      where: { id: dataSourceId },
      data: {
        indexStatus: DataSourceIndexStatus.FAILED,
        data: {
          ...((ds.data as any) || {}),
          error,
        },
      },
    });
  }

  /**
   * Marks a knowledge as failed
   * @param knowledgeId
   * @param error
   */
  public async failDataSourceKnowledge(
    dataSourceId: string,
    knowledgeId: string,
    error: string
  ) {
    const knowledge = await this.knowledgeRepository.getById(knowledgeId);

    const updatedMetadata = this.mergeMetadata(knowledge.metadata, {
      errors: {
        knowledge: error,
      },
    });

    const updatedKnowledge = await this.knowledgeRepository.update(
      knowledgeId,
      {
        indexStatus: KnowledgeIndexStatus.FAILED,
        metadata: updatedMetadata,
      }
    );

    await this.updateDataSourceStatus(dataSourceId);

    return updatedKnowledge;
  }

  /**
   * Marks a knowledge chunk as failed
   * @param knowledgeId
   * @param error
   */
  public async failDataSourceKnowledgeChunk(
    dataSourceId: string,
    knowledgeId: string,
    chunkNumber: number,
    error: string
  ) {
    this.persistChunkLoadingResult(knowledgeId, {
      chunkNumber,
      status: KnowledgeChunkStatus.FAILED,
      error,
    });

    const updatedKnowledge = await this.updateKnowledgeStatus(knowledgeId);
    await this.updateDataSourceStatus(dataSourceId);
    return updatedKnowledge;
  }

  public async deleteUnusedKnowledges() {
    return await this.dataSourceRepository.deleteUnusedKnowledges();
  }

  private mergeMetadata(currentMetadata: any, newMetadata: any) {
    if (currentMetadata && typeof currentMetadata === "object") {
      return {
        ...currentMetadata,
        ...newMetadata,
      };
    }

    return newMetadata;
  }

  public async deleteBlobStorage(knowledgeId: string) {
    const knowledge = await this.knowledgeRepository.getById(knowledgeId);
    if (knowledge.isBlobStorageDeleted) {
      return;
    }

    try {
      if (knowledge.documentsBlobUrl) {
        await FileStorageService.delete(knowledge.documentsBlobUrl);
      }
      if (knowledge.originalContent?.contentBlobUrl) {
        await FileStorageService.delete(
          knowledge.originalContent.contentBlobUrl
        );
      }
      await this.knowledgeRepository.update(knowledgeId, {
        isBlobStorageDeleted: true,
      });
    } catch (e) {
      console.error(
        `Error deleting blob storage for knowledge ${knowledgeId}`,
        e
      );
    }
  }
}

const dataSourceRepository = new DataSourceRepositoryImpl();
const knowledgeRepository = new KnowledgeRepositoryImpl();
const dataSourceManagementService = new DataSourceManagementService(
  dataSourceRepository,
  knowledgeRepository
);
export default dataSourceManagementService;
