import { AIDetailDto, AIProfile } from "@/src/domain/models/AI";
import { AIModelOptions } from "@/src/domain/models/AIModel";
import { AIVisibility } from "@prisma/client";

export interface ListAIsResponse {
  data: AIDetailDto[];
}

export interface CreateAIRequest extends AIRequest {
  userName: string;
}

export interface UpdateAIRequest extends AIRequest {}

export interface AIRequest {
  categoryId: string;
  src: string;
  name: string;
  description: string;
  instructions: string;
  seed: string;
  modelId: string;
  visibility: AIVisibility;
  options: AIModelOptions;
  groups: string[];
  profile: AIProfile;
}
