import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { KnowledgeDto } from "@/src/domain/models/DataSources";
import { KnowledgeRepository } from "@/src/domain/ports/outgoing/KnowledgeRepository";
import prismadb from "@/src/lib/prismadb";
import { Knowledge, Prisma } from "@prisma/client";
import { KnowledgeOriginalContent } from "../knowledge/types/DataSourceTypes";

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
}
