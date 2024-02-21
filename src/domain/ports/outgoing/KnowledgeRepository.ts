import { KnowledgeDto } from "../../models/DataSources";

export interface KnowledgeRepository {
  findById(id: string): Promise<KnowledgeDto | null>;
  getById(id: string): Promise<KnowledgeDto>;
}
