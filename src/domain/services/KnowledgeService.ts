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

  public async findDeletedKnowledgeWithVectorStorage(): Promise<string[]> {
    return await this.knowledgeRepository.findDeletedKnowledgeIdsWithVectorStorage();
  }

  public async setVectorStorageAsDeleted(knowledgeId: string): Promise<void> {
    await this.knowledgeRepository.update(knowledgeId, {
      isVectorStorageDeleted: true,
    });
  }
}

const knowledgeRepository = new KnowledgeRepositoryImpl();
const knowledgeService = new KnowledgeService(knowledgeRepository);
export default knowledgeService;
