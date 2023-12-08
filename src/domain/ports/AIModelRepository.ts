import { AIModel } from "../models/AIModel";

export interface AIModelRepository {
  findAll(): Promise<AIModel[]>;
  findById(id: string): AIModel | null;
}
