import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import {
  KnowledgeChunkCounts,
  KnowledgeChunkDto,
  KnowledgeCounts,
  KnowledgeDto,
  KnowledgeSummary,
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
import {
  KnowledgeChunkEvent,
  KnowledgeChunkIndexes,
} from "../knowledge/types/KnowledgeChunkTypes";

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

  public async getKnowledgeChunkByNumber(
    knowledgeId: string,
    chunkNumber: number
  ): Promise<KnowledgeChunkDto> {
    const knowledgeChunk = await prismadb.knowledgeChunk.findUnique({
      where: {
        knowledgeId_chunkNumber: {
          knowledgeId,
          chunkNumber,
        },
      },
    });

    if (!knowledgeChunk) {
      throw new EntityNotFoundError(
        `Knowledge chunk with knowledgeId ${knowledgeId} and chunkNumber ${chunkNumber} not found`
      );
    }

    return knowledgeChunk;
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

  public async persistKnowledgeChunks(
    knowledgeId: string,
    knowledgeChunkIndexes: KnowledgeChunkIndexes[]
  ): Promise<void> {
    for (const chunk of knowledgeChunkIndexes) {
      await prismadb.knowledgeChunk.upsert({
        where: {
          knowledgeId_chunkNumber: {
            knowledgeId,
            chunkNumber: chunk.chunkNumber,
          },
        },
        update: {
          startIndex: chunk.startIndex,
          endIndex: chunk.endIndex,
        },
        create: {
          knowledgeId,
          chunkNumber: chunk.chunkNumber,
          startIndex: chunk.startIndex,
          endIndex: chunk.endIndex,
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
      take: 500,
    });

    return deletedKnowledgeIds.map((knowledge) => knowledge.id);
  }

  public async findCompletedKnowledgeWithRelatedInstances(): Promise<string[]> {
    const result = await prismadb.$queryRaw<{ id: string }[]>`
    SELECT k.id
    FROM knowledge k
    WHERE k.index_status = 'COMPLETED'
      AND EXISTS
        (SELECT 1
        FROM knowledge kr
        WHERE k.unique_id = kr.unique_id
          AND k.type = kr.type
          AND k.id != kr.id
          AND kr.created_at < k.created_at
          AND kr.index_status != 'DELETED');
    `;

    return result.map((knowledge) => knowledge.id);
  }

  public async getAiKnowledgeSummary(aiId: string): Promise<KnowledgeSummary> {
    const knowledgeList = await prismadb.knowledge.findMany({
      select: {
        id: true,
        documentCount: true,
        tokenCount: true,
      },
      where: {
        indexStatus: {
          in: [
            KnowledgeIndexStatus.COMPLETED,
            KnowledgeIndexStatus.PARTIALLY_COMPLETED,
          ],
        },
        dataSources: {
          some: {
            dataSource: {
              ais: {
                some: {
                  aiId,
                },
              },
            },
          },
        },
      },
    });

    const summary: KnowledgeSummary = knowledgeList.reduce(
      (acc: KnowledgeSummary, knowledge) => {
        return {
          tokenCount: acc.tokenCount + (knowledge.tokenCount || 0),
          documentCount: acc.documentCount + (knowledge.documentCount || 0),
          knowledgeIds: [...acc.knowledgeIds, knowledge.id],
        };
      },
      {
        tokenCount: 0,
        documentCount: 0,
        knowledgeIds: [],
      }
    );

    return summary;
  }
}
