import { UpdateDataSourceRequest } from "@/src/adapter-in/api/DataSourcesApi";
import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import fileLoader from "@/src/adapter-out/knowledge/knowledgeLoaders/FileLoader";
import { ChunkLoadingResult } from "@/src/adapter-out/knowledge/types/ChunkLoadingResult";
import {
  DataSourceItemList,
  KnowledgeOriginalContent,
  RetrieveContentResponseStatus,
} from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import { IndexKnowledgeResponse } from "@/src/adapter-out/knowledge/types/IndexKnowledgeResponse";
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
  KnowledgeIndexStatus,
  PrismaClient,
} from "@prisma/client";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import {
  DataSourceInitializedPayload,
  DataSourceRefreshRequestedPayload,
  DomainEvent,
  KnowledgeChunkReceivedPayload,
} from "../events/domain-event";
import { DataSourceDto, KnowledgeDto } from "../models/DataSources";
import { DataSourceRepository } from "../ports/outgoing/DataSourceRepository";
import { KnowledgeRepository } from "../ports/outgoing/KnowledgeRepository";
import dataSourceAdapterService from "./DataSourceAdapterService";
import { FileStorageService } from "./FileStorageService";
import usageService from "./UsageService";

const KNOWLEDGE_CHUNK_TOKEN_COUNT = 1000;

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
      metadata,
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
    const { originalContent } = knowledge;
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

  public async publishKnowledgeChunkEvents(knowledgeId: string) {
    const knowledge = await this.knowledgeRepository.getById(knowledgeId);

    const { documentsBlobUrl } = knowledge;
    if (!documentsBlobUrl) {
      throw new Error(
        `Knowledge with id=${knowledgeId} does not have documents blob url`
      );
    }

    const documentsJson = await FileStorageService.getJson(documentsBlobUrl);
    const documents: Document[] = documentsJson;

    let chunks: Document[][] = this.getDocumentChunks(documents);
    const chunkCount = chunks.length;

    const chunkMetadata = {
      chunkCount,
      completedChunks: [],
    };

    const metadataWithChunkInfo = this.mergeMetadata(
      knowledge.metadata,
      chunkMetadata
    );
    await this.knowledgeRepository.update(knowledgeId, {
      indexStatus: KnowledgeIndexStatus.INDEXING,
      metadata: metadataWithChunkInfo,
    });

    let eventIds: string[] = [];
    for (let i = 0; i < chunks.length; i++) {
      const eventResult = await this.publishKnowledgeChunkEvent(
        knowledgeId,
        chunks[i],
        i,
        chunkCount
      );
      eventIds = eventIds.concat(eventResult.ids);
    }

    const metadataWithEventIds = this.mergeMetadata(metadataWithChunkInfo, {
      eventIds,
    });
    await this.knowledgeRepository.update(knowledgeId, {
      metadata: metadataWithEventIds,
    });
  }

  private getDocumentChunks(documents: Document[]) {
    let chunks: Document[][] = [];
    let currentChunk: Document[] = [];
    let currentTokenCount = 0;

    for (const document of documents) {
      // Check if adding the current document exceeds the limit
      // And if currentTokenCount is not 0 (the batch is not empty)
      if (
        currentTokenCount + document.metadata.tokenCount >
          KNOWLEDGE_CHUNK_TOKEN_COUNT &&
        currentTokenCount !== 0
      ) {
        // Start a new batch
        chunks.push(currentChunk);
        currentChunk = [];
        currentTokenCount = 0;
      }

      currentChunk.push(document);
      currentTokenCount += document.metadata.tokenCount;
    }

    // Add the last batch if it has documents
    if (currentChunk.length > 0) {
      chunks.push(currentChunk);
    }

    return chunks;
  }

  private async publishKnowledgeChunkEvent(
    knowledgeId: string,
    chunk: Document[],
    chunkNumber: number,
    chunkCount: number
  ) {
    const payload: KnowledgeChunkReceivedPayload = {
      knowledgeId,
      chunk,
      chunkNumber,
      chunkCount,
    };
    return await publishEvent(DomainEvent.KNOWLEDGE_CHUNK_RECEIVED, payload);
  }

  public async loadKnowledgeChunk(chunk: Document[], chunkNumber: number) {
    return await fileLoader.loadDocs(chunk, chunkNumber);
  }

  public async updateDataSourceStatus(
    dataSourceId: string,
    tx: PrismaClient = prismadb
  ) {
    const dataSource = await tx.dataSource.findUnique({
      where: { id: dataSourceId },
      include: {
        knowledges: {
          include: {
            knowledge: true,
          },
        },
      },
    });

    if (!dataSource) {
      return;
    }

    const knowledgeCount = dataSource.knowledges.length;
    let partiallyCompletedKnowledges = 0,
      partiallyCompletedPercents = 0,
      indexingKnowledges = 0,
      completedKnowledges = 0,
      failedKnowledges = 0,
      totalDocumentCount = 0,
      totalTokenCount = 0;
    for (const { knowledge } of dataSource.knowledges) {
      totalDocumentCount += knowledge.documentCount ?? 0;
      totalTokenCount += knowledge.tokenCount ?? 0;

      switch (knowledge.indexStatus) {
        case KnowledgeIndexStatus.INITIALIZED:
        case KnowledgeIndexStatus.RETRIEVING_CONTENT:
        case KnowledgeIndexStatus.CONTENT_RETRIEVED:
        case KnowledgeIndexStatus.DOCUMENTS_CREATED:
        case KnowledgeIndexStatus.INDEXING:
          indexingKnowledges++;
          break;
        case KnowledgeIndexStatus.COMPLETED:
          completedKnowledges++;
          break;
        case KnowledgeIndexStatus.PARTIALLY_COMPLETED:
          partiallyCompletedKnowledges++;
          partiallyCompletedPercents +=
            ((knowledge.metadata as any)?.percentComplete || 0) / 100;
          break;
        case KnowledgeIndexStatus.FAILED:
          failedKnowledges++;
          break;
      }
    }

    let indexPercentage;
    if (knowledgeCount === 0) {
      indexPercentage = 100;
    } else {
      indexPercentage =
        (((partiallyCompletedKnowledges === 0
          ? 0
          : partiallyCompletedPercents / partiallyCompletedKnowledges) +
          completedKnowledges) /
          knowledgeCount) *
        100;
    }

    let indexingStatus;
    if (indexingKnowledges > 0) {
      indexingStatus =
        dataSource.indexStatus === DataSourceIndexStatus.REFRESHING
          ? DataSourceIndexStatus.REFRESHING
          : DataSourceIndexStatus.INDEXING;
    } else if (failedKnowledges === knowledgeCount) {
      indexingStatus = DataSourceIndexStatus.FAILED;
    } else if (completedKnowledges === knowledgeCount) {
      indexingStatus = DataSourceIndexStatus.COMPLETED;
    } else {
      indexingStatus = DataSourceIndexStatus.PARTIALLY_COMPLETED;
    }

    const lastIndexedAt =
      indexingStatus === DataSourceIndexStatus.INDEXING ? null : new Date();

    await tx.dataSource.update({
      where: { id: dataSource.id },
      data: {
        indexStatus: indexingStatus,
        indexPercentage,
        lastIndexedAt,
        documentCount: totalDocumentCount,
        tokenCount: totalTokenCount,
      },
    });
  }

  public async persistChunkLoadingResult(
    knowledgeId: string,
    chunkLoadingResult: ChunkLoadingResult
  ): Promise<KnowledgeDto> {
    const knowledge = await knowledgeRepository.getById(knowledgeId);

    const currentMeta = knowledge.metadata as any;
    const { completedChunks } = currentMeta;
    const { chunkNumber, chunkCount } = chunkLoadingResult;

    completedChunks.push(chunkNumber);

    const uniqCompletedChunks = new Set(completedChunks);
    const percentComplete = (uniqCompletedChunks.size / chunkCount) * 100;

    let indexStatus: KnowledgeIndexStatus | undefined;
    if (chunkCount === uniqCompletedChunks.size) {
      indexStatus = KnowledgeIndexStatus.COMPLETED;
    }
    console.log(
      `Knowledge ${knowledgeId}: ${uniqCompletedChunks.size} / ${chunkCount} loaded`
    );

    const updatedMetadata = this.mergeMetadata(currentMeta, {
      completedChunks: Array.from(uniqCompletedChunks),
      percentComplete,
      documentIds: chunkLoadingResult.docIds,
    });

    const updatedKnowledge = await this.knowledgeRepository.update(
      knowledgeId,
      {
        lastIndexedAt: new Date(),
        indexStatus,
        metadata: updatedMetadata,
      }
    );

    await this.updateCompletedKnowledgeDataSources(knowledgeId);

    return updatedKnowledge;
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
    const knowledgeList = await prismadb.knowledge.findMany({
      where: {
        indexStatus: {
          in: [KnowledgeIndexStatus.INITIALIZED, KnowledgeIndexStatus.INDEXING],
        },
        dataSources: {
          some: {
            dataSourceId: dataSourceId,
          },
        },
      },
    });

    const dataSourceAdapter =
      dataSourceAdapterService.getDataSourceAdapter(dataSourceType);
    for (const knowledge of knowledgeList) {
      const indexKnowledgeResponse =
        await dataSourceAdapter.pollKnowledgeIndexingStatus(knowledge);

      await this.persistIndexedKnowledge(knowledge, indexKnowledgeResponse);
    }

    await this.updateDataSourceStatus(dataSourceId);
  }

  private async persistIndexedKnowledge(
    knowledge: Knowledge,
    indexKnowledgeResponse: IndexKnowledgeResponse,
    tx: PrismaClient = prismadb
  ) {
    let updateDataForKnowledge: {
      indexStatus: KnowledgeIndexStatus;
      blobUrl: string | null;
      lastIndexedAt: Date;
      documentCount?: number;
      tokenCount?: number;
      metadata?: any;
    } = {
      indexStatus: indexKnowledgeResponse.indexStatus,
      blobUrl: knowledge.blobUrl || indexKnowledgeResponse.blobUrl || null,
      lastIndexedAt: new Date(),
      documentCount: indexKnowledgeResponse.documentCount,
      tokenCount: indexKnowledgeResponse.tokenCount,
    };

    if (indexKnowledgeResponse.metadata) {
      const currentKnowledge = await tx.knowledge.findUnique({
        where: { id: knowledge.id },
        select: { metadata: true },
      });

      if (!currentKnowledge) {
        return;
      }

      const currentMetadata =
        currentKnowledge.metadata &&
        typeof currentKnowledge.metadata === "object"
          ? currentKnowledge.metadata
          : {};

      // Merge existing metadata with the new metadata
      updateDataForKnowledge.metadata = {
        ...currentMetadata,
        ...indexKnowledgeResponse.metadata,
      };
    }

    await tx.knowledge.update({
      where: { id: knowledge.id },
      data: updateDataForKnowledge,
    });
  }

  private async updateCompletedKnowledgeDataSources(
    knowledgeId: string,
    tx: PrismaClient = prismadb
  ) {
    const dataSourceIds = await tx.dataSource.findMany({
      select: { id: true },
      where: {
        knowledges: { some: { knowledgeId } },
      },
    });

    for (const dataSource of dataSourceIds) {
      await this.updateDataSourceStatus(dataSource.id, tx);
    }
  }

  /**
   * Creates a copy of an existing knowledge and copies over
   * all existing data source associations
   * @param knowledge
   * @returns
   */
  public async copyKnowledgeAndAssociations(knowledgeId: string) {
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
      include: {
        dataSources: true,
      },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }

    const newKnowledge = await prismadb.knowledge.create({
      data: {
        name: knowledge.name,
        type: knowledge.type,
        uniqueId: knowledge.uniqueId,
        indexStatus: KnowledgeIndexStatus.INITIALIZED,
        blobUrl: knowledge.blobUrl,
        metadata: knowledge.metadata as any,
        isMigrated: true,
      },
    });

    const dataSourceKnowledgeRelations = knowledge.dataSources.map(
      (existingAssociation) => {
        return {
          dataSourceId: existingAssociation.dataSourceId,
          knowledgeId: newKnowledge.id,
        };
      }
    );

    await prismadb.dataSourceKnowledge.createMany({
      data: dataSourceKnowledgeRelations,
    });

    return newKnowledge;
  }

  public async deleteRelatedKnowledgeInstances(
    knowledgeId: string
  ): Promise<string[]> {
    const knowledge = await knowledgeRepository.getById(knowledgeId);

    if (!knowledge.uniqueId) {
      return [];
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
      return [];
    }

    const relatedAndNewKnowledgeIds = [...relatedKnowledgeIds, knowledgeId];

    // Find all data sources which are associated with the related or the new knowledge instances
    const relatedDataSources = await prismadb.dataSourceKnowledge.findMany({
      select: { dataSourceId: true },
      distinct: ["dataSourceId"],
      where: {
        knowledgeId: { in: relatedAndNewKnowledgeIds },
      },
    });

    // Create new data source knowledge relationships for the new knowledge instance
    const newDataSourceRelationships = relatedDataSources.map((dataSource) => {
      return {
        dataSourceId: dataSource.dataSourceId,
        knowledgeId,
      };
    });

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

    return relatedKnowledgeIds;
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
    dataSourceId: string
  ) {
    const dataSource = await this.getDataSource(dataSourceId);

    const canUpdateDataSource = DataSourceSecurityService.canUpdateDataSource(
      authorizationContext,
      dataSource
    );

    if (!canUpdateDataSource) {
      throw new ForbiddenError("Forbidden");
    }

    await this.refreshDataSourceAndPublishEvent(dataSource);
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
  public async failDataSourceKnowledge(knowledgeId: string, error: string) {
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }

    const currentMeta = knowledge.metadata as any;
    const indexKnowledgeResponse = {
      indexStatus: KnowledgeIndexStatus.FAILED,
      metadata: {
        errors: {
          knowledge: error,
          ...(currentMeta?.errors || {}),
        },
      },
    };

    await this.persistIndexedKnowledge(knowledge, indexKnowledgeResponse);
    await this.updateCompletedKnowledgeDataSources(knowledge.id);

    return indexKnowledgeResponse;
  }

  /**
   * Marks a knowledge chunk as failed
   * @param knowledgeId
   * @param error
   */
  public async failDataSourceKnowledgeChunk(
    knowledgeId: string,
    chunkIndex: number,
    error: string
  ) {
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }
    const currentMeta = knowledge.metadata as any;
    const errors = {
      ...(currentMeta?.errors || {}),
    };
    errors[`chunk-${chunkIndex}`] = error;
    const indexKnowledgeResponse = {
      indexStatus: KnowledgeIndexStatus.FAILED,
      metadata: { errors },
    };

    await this.persistIndexedKnowledge(knowledge, indexKnowledgeResponse);
    await this.updateCompletedKnowledgeDataSources(knowledge.id);

    return indexKnowledgeResponse;
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
}

const dataSourceRepository = new DataSourceRepositoryImpl();
const knowledgeRepository = new KnowledgeRepositoryImpl();
const dataSourceManagementService = new DataSourceManagementService(
  dataSourceRepository,
  knowledgeRepository
);
export default dataSourceManagementService;
