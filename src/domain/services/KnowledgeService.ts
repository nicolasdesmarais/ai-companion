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
}

const knowledgeRepository = new KnowledgeRepositoryImpl();
const knowledgeService = new KnowledgeService(knowledgeRepository);
export default knowledgeService;
