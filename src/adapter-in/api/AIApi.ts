import { AIRequest } from "@/src/domain/models/AI";

export interface CreateAIRequest extends AIRequest {
  userName: string;
}

export interface UpdateAIRequest extends AIRequest {}
