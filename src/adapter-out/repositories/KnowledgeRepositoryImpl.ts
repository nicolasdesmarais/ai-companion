import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import {
  KnowledgeChunkCounts,
  KnowledgeCounts,
  KnowledgeDto,
} from "@/src/domain/models/DataSources";
import { KnowledgeRepository } from "@/src/domain/ports/outgoing/KnowledgeRepository";
import prismadb from "@/src/lib/prismadb";
import {
  Knowledge,
  KnowledgeChunkStatus,
  KnowledgeIndexStatus,
  Prisma,
} from "@prisma/client";
import { KnowledgeOriginalContent } from "../knowledge/types/DataSourceTypes";
import { KnowledgeChunkEvent } from "../knowledge/types/KnowledgeChunkTypes";

const mapKnowledgeToDto = (knowledge: Knowledge): KnowledgeDto => {
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
    indexPercentage,
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
    indexPercentage: indexPercentage.toString(),
    metadata,
  };
};

export class KnowledgeRepositoryImpl implements KnowledgeRepository {
  public async findById(id: string): Promise<KnowledgeDto | null> {
    const knowledge = await prismadb.knowledge.findUnique({
      where: { id },
    });

    if (!knowledge) {
      return null;
    }

    return mapKnowledgeToDto(knowledge);
  }

  public async getById(id: string): Promise<KnowledgeDto> {
    const knowledge = await this.findById(id);
    if (!knowledge) {
      throw new EntityNotFoundError(`Knowledge with id ${id} not found`);
    }
    return knowledge;
  }

  public async update(
    id: string,
    input: Prisma.KnowledgeUpdateInput
  ): Promise<KnowledgeDto> {
    const updatedKnowledge = await prismadb.knowledge.update({
      where: { id },
      data: input,
    });
    return mapKnowledgeToDto(updatedKnowledge);
  }

  public async initializeKnowledgeChunks(
    knowledgeId: string,
    chunkCount: number
  ): Promise<void> {
    for (let i = 0; i < chunkCount; i++) {
      const chunkNumber = i;
      await prismadb.knowledgeChunk.upsert({
        where: {
          knowledgeId_chunkNumber: {
            knowledgeId,
            chunkNumber,
          },
        },
        update: {},
        create: {
          knowledgeId,
          chunkNumber,
          status: KnowledgeChunkStatus.INDEXING,
        },
      });
    }
  }

  public async persistKnowledgeChunkEvents(
    knowledgeId: string,
    chunks: KnowledgeChunkEvent[]
  ): Promise<void> {
    for (const chunk of chunks) {
      await prismadb.knowledgeChunk.update({
        where: {
          knowledgeId_chunkNumber: {
            knowledgeId,
            chunkNumber: chunk.chunkNumber,
          },
        },
        data: {
          eventId: chunk.eventId,
        },
      });
    }
  }

  public async updateKnowledgeChunkStatus(
    knowledgeId: string,
    chunkNumber: number,
    status: KnowledgeChunkStatus,
    error?: string
  ): Promise<void> {
    await prismadb.knowledgeChunk.update({
      where: {
        knowledgeId_chunkNumber: {
          knowledgeId,
          chunkNumber,
        },
      },
      data: { status, error },
    });
  }

  public async getKnowledgeChunkCounts(
    knowledgeId: string
  ): Promise<KnowledgeChunkCounts> {
    const results = await prismadb.knowledgeChunk.groupBy({
      by: ["status"],
      where: {
        knowledgeId: knowledgeId,
      },
      _count: {
        _all: true,
      },
    });

    let totalCount = 0;
    let completedCount = 0;
    let failedCount = 0;

    results.forEach((result) => {
      totalCount += result._count._all;
      if (result.status === KnowledgeChunkStatus.COMPLETED) {
        completedCount += result._count._all;
      } else if (result.status === KnowledgeChunkStatus.FAILED) {
        failedCount += result._count._all;
      }
    });

    return {
      totalCount,
      completedCount,
      failedCount,
    };
  }

  public async getKnowledgeCounts(
    dataSourceId: string
  ): Promise<KnowledgeCounts> {
    const results = await prismadb.knowledge.groupBy({
      by: ["indexStatus"],
      where: {
        indexStatus: {
          not: KnowledgeIndexStatus.DELETED,
        },
        dataSources: {
          some: {
            dataSourceId,
          },
        },
      },
      _count: {
        _all: true,
      },
      _sum: {
        documentCount: true,
        tokenCount: true,
        indexPercentage: true,
      },
    });

    let totalCount = 0;
    let indexingCount = 0;
    let completedCount = 0;
    let partiallyCompletedCount = 0;
    let failedCount = 0;
    let totalDocumentCount = 0;
    let totalTokenCount = 0;
    let indexPercentageSum = 0;

    results.forEach((result) => {
      totalCount += result._count._all;
      totalDocumentCount += result._sum.documentCount || 0;
      totalTokenCount += result._sum.tokenCount || 0;
      indexPercentageSum += result._sum.indexPercentage?.toNumber() || 0;

      switch (result.indexStatus) {
        case KnowledgeIndexStatus.COMPLETED:
          completedCount += result._count._all;
          break;
        case KnowledgeIndexStatus.PARTIALLY_COMPLETED:
          partiallyCompletedCount += result._count._all;
          break;
        case KnowledgeIndexStatus.FAILED:
          failedCount += result._count._all;
          break;
        default:
          indexingCount += result._count._all;
          break;
      }
    });

    const indexPercentage =
      totalCount !== 0 ? indexPercentageSum / totalCount : 0;

    return {
      totalCount,
      indexingCount,
      completedCount,
      partiallyCompletedCount,
      failedCount,
      totalDocumentCount,
      totalTokenCount,
      indexPercentage,
    };
  }

  public async findDeletedKnowledgeIdsWithBlobStorageUrl(): Promise<string[]> {
    const deletedKnowledgeIds = await prismadb.knowledge.findMany({
      where: {
        indexStatus: KnowledgeIndexStatus.DELETED,
        isBlobStorageDeleted: false,
      },
      select: {
        id: true,
      },
      take: 1000,
    });

    return deletedKnowledgeIds.map((knowledge) => knowledge.id);
  }
}
