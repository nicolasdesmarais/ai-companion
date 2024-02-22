import { Prisma } from "@prisma/client";
import { KnowledgeDto } from "../../models/DataSources";

export interface KnowledgeRepository {
  findById(id: string): Promise<KnowledgeDto | null>;
  getById(id: string): Promise<KnowledgeDto>;
  update(id: string, input: Prisma.KnowledgeUpdateInput): Promise<KnowledgeDto>;
}
