import { publishEvent } from "@/src/adapter-in/inngest/event-publisher";
import apiDataSourceAdapter from "@/src/adapter-out/knowledge/api/ApiDataSourceAdapter";
import fileUploadDataSourceAdapter from "@/src/adapter-out/knowledge/file-upload/FileUploadDataSourceAdapter";
import googleDriveDataSourceAdapter from "@/src/adapter-out/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import { DataSourceAdapter } from "@/src/adapter-out/knowledge/types/DataSourceAdapter";
import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceItemList";
import { IndexKnowledgeResponse } from "@/src/adapter-out/knowledge/types/IndexKnowledgeResponse";
import { KnowledgeIndexingResult } from "@/src/adapter-out/knowledge/types/KnowlegeIndexingResult";
import webUrlsDataSourceAdapter from "@/src/adapter-out/knowledge/web-urls/WebUrlsDataSourceAdapter";
import { DataSourceRepositoryImpl } from "@/src/adapter-out/repositories/DataSourceRepositoryImpl";
import prismadb from "@/src/lib/prismadb";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { AISecurityService } from "@/src/security/services/AISecurityService";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";
import { DataSourceSecurityService } from "@/src/security/services/DataSourceSecurityService";
import {
  DataSourceIndexStatus,
  DataSourceRefreshPeriod,
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
  PrismaClient,
} from "@prisma/client";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import { DomainEvent } from "../events/domain-event";
import { DataSourceDto } from "../models/DataSources";
import { DataSourceRepository } from "../ports/outgoing/DataSourceRepository";
import msftDataSourceAdapter from "@/src/adapter-out/knowledge/msft/MsftDataSourceAdapter";

export class DataSourceService {
  constructor(private dataSourceRepository: DataSourceRepository) {}

  private getDataSourceAdapter(type: DataSourceType): DataSourceAdapter {
    switch (type) {
      case DataSourceType.GOOGLE_DRIVE:
        return googleDriveDataSourceAdapter;
      case DataSourceType.WEB_URL:
        return webUrlsDataSourceAdapter;
      case DataSourceType.FILE_UPLOAD:
        return fileUploadDataSourceAdapter;
      case DataSourceType.API:
        return apiDataSourceAdapter;
      case DataSourceType.ONEDRIVE:
        return msftDataSourceAdapter;
      default:
        throw new Error(`DataSourceType ${type} not supported`);
    }
  }

  private async getDataSourceAndAdapter(dataSourceId: string) {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
    });

    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    const dataSourceAdapter = this.getDataSourceAdapter(dataSource.type);
    return { dataSource, dataSourceAdapter };
  }

  /**
   * Returns a list of all data sources the user has access to.
   * @param authorizationContext
   * @returns
   */
  public async listDataSources(
    authorizationContext: AuthorizationContext
  ): Promise<DataSourceDto[]> {
    const highestAccessLevel = BaseEntitySecurityService.getHighestAccessLevel(
      authorizationContext,
      SecuredResourceType.DATA_SOURCES,
      SecuredAction.READ
    );
    if (!highestAccessLevel) {
      throw new ForbiddenError("Forbidden");
    }

    const { orgId, userId } = authorizationContext;
    switch (highestAccessLevel) {
      case SecuredResourceAccessLevel.INSTANCE:
        return await this.dataSourceRepository.findAll();
      case SecuredResourceAccessLevel.ORGANIZATION:
        return await this.dataSourceRepository.findByOrgId(orgId);
      case SecuredResourceAccessLevel.SELF:
        return await this.dataSourceRepository.findByOrgIdAndUserId(
          orgId,
          userId
        );
    }
  }

  /**
   * Returns a list of all data sources for the specified AI.
   * @param authorizationContext
   * @param aiId
   * @returns
   */
  public async listAIDataSources(
    authorizationContext: AuthorizationContext,
    aiId: string
  ): Promise<DataSourceDto[]> {
    const ai = await prismadb.aI.findUnique({
      where: { id: aiId },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const canReadAi = AISecurityService.canReadAI(authorizationContext, ai);
    if (!canReadAi) {
      throw new ForbiddenError(
        "User is not authorized to read AI with id=${aiId}"
      );
    }

    return await dataSourceRepository.findByAiId(aiId);
  }

  /**
   * Create and persist a data source entity.
   * Publishes a DATASOURCE_INITIALIZED event.
   * @param authorizationContext
   * @param name
   * @param type
   * @param data
   * @returns
   */
  public async createDataSource(
    authorizationContext: AuthorizationContext,
    name: string,
    type: DataSourceType,
    refreshPeriod: DataSourceRefreshPeriod,
    data: any
  ) {
    const { orgId, userId } = authorizationContext;

    const dataSource = await dataSourceRepository.initializeDataSource(
      orgId,
      userId,
      name,
      type,
      refreshPeriod,
      data
    );

    const dataSourceId = dataSource.id;
    await publishEvent(DomainEvent.DATASOURCE_INITIALIZED, {
      dataSourceId,
      dataSourceType: type,
    });

    return dataSourceId;
  }

  /**
   * Retrieves and persists knowledge list for the specified data source
   * @param dataSourceId
   * @returns
   */
  public async createDataSourceKnowledgeList(dataSourceId: string) {
    const { dataSource, dataSourceAdapter } =
      await this.getDataSourceAndAdapter(dataSourceId);

    const itemList = await dataSourceAdapter.getDataSourceItemList(
      dataSource.orgId,
      dataSource.ownerUserId,
      dataSourceId,
      dataSource.data
    );

    await this.upsertKnowledgeListAndCreateAssociations(
      dataSourceId,
      dataSourceAdapter,
      itemList
    );
  }

  public async updateDataSourceKnowledgeList(dataSourceId: string) {
    const { dataSource, dataSourceAdapter } =
      await this.getDataSourceAndAdapter(dataSourceId);

    const itemList = await dataSourceAdapter.getDataSourceItemList(
      dataSource.orgId,
      dataSource.ownerUserId,
      dataSourceId,
      dataSource.data
    );

    await this.upsertKnowledgeListAndUpdateAssociations(
      dataSourceId,
      dataSourceAdapter,
      itemList
    );
  }

  /**
   * Handle asynchronous receipt of a data source item list through an event
   * @param dataSourceId
   * @param dataSourceItemList
   * @returns
   */
  public async onDataSourceItemListReceived(
    dataSourceId: string,
    dataSourceItemList: DataSourceItemList
  ) {
    const { dataSourceAdapter } = await this.getDataSourceAndAdapter(
      dataSourceId
    );

    const dataSource = await dataSourceRepository.findById(dataSourceId);
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    if (dataSource.indexStatus === DataSourceIndexStatus.REFRESHING) {
      await this.upsertKnowledgeListAndUpdateAssociations(
        dataSourceId,
        dataSourceAdapter,
        dataSourceItemList
      );
    } else {
      await this.upsertKnowledgeListAndCreateAssociations(
        dataSourceId,
        dataSourceAdapter,
        dataSourceItemList
      );
    }
  }

  private async upsertKnowledgeListAndCreateAssociations(
    dataSourceId: string,
    dataSourceAdapter: DataSourceAdapter,
    itemList: DataSourceItemList
  ) {
    const knowledgeList = await this.upsertKnowledgeList(
      dataSourceAdapter,
      itemList
    );

    const dataSourceKnowledgeRelations = knowledgeList.map((knowledge) => {
      return { dataSourceId, knowledgeId: knowledge.id };
    });

    await prismadb.dataSourceKnowledge.createMany({
      data: dataSourceKnowledgeRelations,
    });

    await this.publishKnowledgeEvents(dataSourceId, knowledgeList);
  }

  private async upsertKnowledgeListAndUpdateAssociations(
    dataSourceId: string,
    dataSourceAdapter: DataSourceAdapter,
    itemList: DataSourceItemList
  ) {
    const knowledgeList = await this.upsertKnowledgeList(
      dataSourceAdapter,
      itemList
    );

    const knowledgeIds = knowledgeList.map((knowledge) => knowledge.id);

    // Find existing associations
    const existingAssociations = await prismadb.dataSourceKnowledge.findMany({
      where: {
        dataSourceId: dataSourceId,
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

    await this.publishKnowledgeEvents(dataSourceId, knowledgeList);
  }

  private async upsertKnowledgeList(
    dataSourceAdapter: DataSourceAdapter,
    itemList: DataSourceItemList
  ): Promise<Knowledge[]> {
    if (itemList.items.length === 0) {
      return [];
    }
    const knowledgeList = [];
    const existingKnowledgeMap = await this.getExistingKnowledgeMap(itemList);

    for (const item of itemList.items) {
      let knowledge = existingKnowledgeMap.get(item.uniqueId!);

      if (
        knowledge &&
        dataSourceAdapter.shouldReindexKnowledge(knowledge, item)
      ) {
        knowledge = undefined;
      }

      if (!knowledge) {
        knowledge = await prismadb.knowledge.create({
          data: {
            name: item.name,
            type: itemList.type,
            uniqueId: item.uniqueId,
            indexStatus: KnowledgeIndexStatus.INITIALIZED,
            blobUrl: item.blobUrl,
            metadata: item.metadata,
          },
        });
      }

      knowledgeList.push(knowledge);
    }

    return knowledgeList;
  }

  private async publishKnowledgeEvents(
    dataSourceId: string,
    knowledgeList: Knowledge[]
  ) {
    if (knowledgeList.length === 0) {
      return;
    }

    const knowledgeListToUpdate = knowledgeList.filter(
      (knowledge) => knowledge.indexStatus === KnowledgeIndexStatus.INITIALIZED
    );
    if (knowledgeListToUpdate.length === 0) {
      await this.updateDataSourceStatus(dataSourceId);
      return;
    }

    for (const knowledge of knowledgeListToUpdate) {
      await publishEvent(DomainEvent.KNOWLEDGE_INITIALIZED, {
        dataSourceId,
        knowledgeId: knowledge.id,
      });
    }
  }

  private async getExistingKnowledgeMap(itemList: DataSourceItemList) {
    const existingKnowledgeMap = new Map<string, Knowledge>();
    const uniqueIds = itemList.items
      .map((item) => item.uniqueId)
      .filter((uniqueId): uniqueId is string => uniqueId !== undefined);

    if (uniqueIds.length > 0) {
      const existingKnowledge = await prismadb.knowledge.findMany({
        where: {
          type: itemList.type,
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
   * Indexes a knowledge for the specified data source
   * @param dataSourceId
   * @param knowledgeId
   */
  public async indexDataSourceKnowledge(
    dataSourceId: string,
    knowledgeId: string
  ) {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
    });
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }

    let indexKnowledgeResponse;
    if (knowledge.indexStatus !== KnowledgeIndexStatus.COMPLETED) {
      const dataSourceAdapter = this.getDataSourceAdapter(dataSource.type);
      try {
        indexKnowledgeResponse = await dataSourceAdapter.indexKnowledge(
          dataSource.orgId,
          dataSource.ownerUserId,
          knowledge,
          dataSource.data
        );
      } catch (error) {
        console.error(error);
        indexKnowledgeResponse = {
          indexStatus: KnowledgeIndexStatus.FAILED,
        };
      }

      await this.persistIndexedKnowledge(knowledge, indexKnowledgeResponse);
    }
    await this.updateDataSourceStatus(dataSourceId);
    return indexKnowledgeResponse;
  }

  private async updateDataSourceStatus(
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

  /**
   * Accept data received from a knowledge indexed event and publish a
   * KNOWLEDGE_INDEXED_EVENT_RECEIVED domain event.
   * @param dataSourceType
   * @param data
   */
  public async knowledgeEventReceived(
    dataSourceType: DataSourceType,
    data: any
  ) {
    await publishEvent(DomainEvent.KNOWLEDGE_EVENT_RECEIVED, {
      dataSourceType,
      data,
    });
  }

  /**
   * Updates a knowledge with data received through an event
   *
   * @param dataSourceType
   * @param data
   */
  public async getKnowledgeResultFromEvent(
    dataSourceType: DataSourceType,
    data: any
  ) {
    const dataSourceAdapter = this.getDataSourceAdapter(dataSourceType);
    const knowledgeId = dataSourceAdapter.retrieveKnowledgeIdFromEvent(data);
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }

    const result = await dataSourceAdapter.getKnowledgeResultFromEvent(
      knowledge,
      data
    );
    return {
      knowledgeId: knowledge.id,
      result,
    };
  }

  public async loadKnowledgeResult(
    dataSourceType: DataSourceType,
    knowledgeId: string,
    result: KnowledgeIndexingResult,
    index: number
  ) {
    const dataSourceAdapter = this.getDataSourceAdapter(dataSourceType);
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }

    return await dataSourceAdapter.loadKnowledgeResult(
      knowledge,
      result,
      index
    );
  }

  public async persistIndexingResult(
    knowledgeId: string,
    indexKnowledgeResponse: IndexKnowledgeResponse,
    chunkCount: number
  ) {
    await prismadb.$transaction(async (tx: any) => {
      const knowledge = await tx.knowledge.findUnique({
        where: { id: knowledgeId },
      });
      if (!knowledge) {
        throw new EntityNotFoundError(
          `Knowledge with id=${knowledgeId} not found`
        );
      }
      const meta = indexKnowledgeResponse.metadata as any;
      if (knowledge.metadata) {
        const currentMeta = knowledge.metadata as any;
        if (currentMeta.documentCount) {
          meta.documentCount += currentMeta.documentCount;
        }
        if (currentMeta.totalTokenCount) {
          meta.totalTokenCount += currentMeta.totalTokenCount;
        }
        if (currentMeta.completedChunks) {
          meta.completedChunks = meta.completedChunks.concat(
            currentMeta.completedChunks
          );
        }
      }
      const uniqCompletedChunks = new Set(meta.completedChunks);
      meta.percentComplete = (uniqCompletedChunks.size / chunkCount) * 100;
      if (chunkCount === uniqCompletedChunks.size) {
        indexKnowledgeResponse.indexStatus = KnowledgeIndexStatus.COMPLETED;
      }
      console.log(
        `Knowledge ${knowledgeId}: ${uniqCompletedChunks.size} / ${chunkCount} loaded`
      );

      await this.persistIndexedKnowledge(knowledge, indexKnowledgeResponse, tx);
      await this.updateCompletedKnowledgeDataSources(knowledge.id, tx);
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

    const dataSourceAdapter = this.getDataSourceAdapter(dataSourceType);
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
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id: knowledgeId },
    });

    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${knowledgeId} not found`
      );
    }

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

    const relatedDataSources = await prismadb.dataSourceKnowledge.findMany({
      select: { dataSourceId: true },
      distinct: ["dataSourceId"],
      where: {
        knowledgeId: { in: relatedKnowledgeIds },
      },
    });

    const newDataSourceRelationships = relatedDataSources.map((dataSource) => {
      return {
        dataSourceId: dataSource.dataSourceId,
        knowledgeId,
      };
    });

    await prismadb.$transaction(async (tx) => {
      await tx.dataSourceKnowledge.deleteMany({
        where: {
          knowledgeId: { in: [...relatedKnowledgeIds, knowledgeId] },
        },
      });

      await tx.knowledge.updateMany({
        where: {
          id: { in: relatedKnowledgeIds },
        },
        data: { indexStatus: KnowledgeIndexStatus.DELETED },
      });

      await tx.dataSourceKnowledge.createMany({
        data: newDataSourceRelationships,
      });
    });

    return relatedKnowledgeIds;
  }

  public async deleteDataSource(
    authorizationContext: AuthorizationContext,
    dataSourceId: string
  ) {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
      include: {
        knowledges: true,
      },
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

    const knowledgeIds: string[] = dataSource.knowledges.map(
      (dataSourceKnowledge) => dataSourceKnowledge.knowledgeId
    );

    await prismadb.$transaction(async (tx) => {
      await prismadb.aIDataSource.deleteMany({
        where: { dataSourceId },
      });

      await prismadb.dataSourceKnowledge.deleteMany({
        where: { dataSourceId },
      });

      await prismadb.knowledge.deleteMany({
        where: {
          id: { in: knowledgeIds },
          NOT: {
            indexStatus: {
              in: [
                KnowledgeIndexStatus.COMPLETED,
                KnowledgeIndexStatus.PARTIALLY_COMPLETED,
              ],
            },
          },
        },
      });

      await prismadb.dataSource.delete({ where: { id: dataSourceId } });
    });
  }

  public async findDataSourcesToRefresh() {
    return await dataSourceRepository.findDataSourceIdsToRefresh(new Date());
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

  public async refreshDataSourceAsSystem(dataSourceId: string) {
    const dataSource = await dataSourceRepository.findById(dataSourceId);
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    await this.refreshDataSourceAndPublishEvent(dataSource);
  }

  private async getDataSource(dataSourceId: string) {
    const dataSource = await dataSourceRepository.findById(dataSourceId);
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }
    return dataSource;
  }

  private async refreshDataSourceAndPublishEvent(dataSource: DataSourceDto) {
    dataSource.indexStatus = DataSourceIndexStatus.REFRESHING;
    await dataSourceRepository.updateDataSource(dataSource);
    await publishEvent(DomainEvent.DATASOURCE_REFRESH_REQUESTED, {
      dataSourceId: dataSource.id,
    });
  }
}

const dataSourceRepository = new DataSourceRepositoryImpl();
const dataSourceService = new DataSourceService(dataSourceRepository);
export default dataSourceService;
