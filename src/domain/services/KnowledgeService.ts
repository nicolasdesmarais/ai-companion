import { KnowledgeRepositoryImpl } from "@/src/adapter-out/repositories/KnowledgeRepositoryImpl";
import { KnowledgeDto, KnowledgeSummary } from "../models/DataSources";
import { KnowledgeRepository } from "../ports/outgoing/KnowledgeRepository";

export class KnowledgeService {
  constructor(private knowledgeRepository: KnowledgeRepository) {}

  public async getKnowledge(knowledgeId: string): Promise<KnowledgeDto> {
    return await this.knowledgeRepository.getById(knowledgeId);
  }

  public async getAiKnowledgeSummary(aiId: string): Promise<KnowledgeSummary> {
    return await this.knowledgeRepository.getAiKnowledgeSummary(aiId);
  }

  public async findCompletedKnowledgeWithRelatedInstances(): Promise<string[]> {
    return await this.knowledgeRepository.findCompletedKnowledgeWithRelatedInstances();
  }

  public async findDeletedKnowledgeWithBlobStorage(): Promise<string[]> {
    return await this.knowledgeRepository.findDeletedKnowledgeIdsWithBlobStorageUrl();
  }

  public async findDeletedKnowledgeWithVectorStorage(
    limit?: number
  ): Promise<string[]> {
    return await this.knowledgeRepository.findDeletedKnowledgeIdsWithVectorStorage(
      limit
    );
  }

  public async findFailedKnowledge(limit?: number): Promise<string[]> {
    return await this.knowledgeRepository.findFailedKnowledge(limit);
  }

  public async setVectorStorageAsDeleted(knowledgeId: string): Promise<void> {
    await this.knowledgeRepository.update(knowledgeId, {
      isVectorStorageDeleted: true,
    });
  }

  public async resetKnowledgeChunks(
    knowledgeId: string
  ): Promise<KnowledgeDto> {
    await this.knowledgeRepository.deleteKnowledgeChunks(knowledgeId);

    const knowledge = await this.knowledgeRepository.getById(knowledgeId);
    const updatedMetadata = this.mergeMetadata(knowledge.metadata, {
      chunkCount: 0,
    });

    return await this.knowledgeRepository.update(knowledgeId, {
      isVectorStorageDeleted: false,
      metadata: updatedMetadata,
    });
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

const knowledgeRepository = new KnowledgeRepositoryImpl();
const knowledgeService = new KnowledgeService(knowledgeRepository);
export default knowledgeService;
