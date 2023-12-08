import { AIVisibility } from "@prisma/client";
import { AIModelOptions } from "../../models/AIModel";

export interface CreateAIRequest extends AIRequest {
  orgId: string;
  userId: string;
  userName: string;
}

export interface UpdateAIRequest extends AIRequest {}

export interface AIProfileFeature {
  title: string;
  description: string;
}
export interface AIProfile {
  headline: string;
  description: string;
  features: AIProfileFeature[];
  showCharacter: boolean;
  showTraining: boolean;
  showPersonality: boolean;
}

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

export interface ListAIsResponse {
  data: ListAIDto[];
}

export interface ListAIDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  description: string;
  src: string;
  profile: AIProfile;
  userId: string;
  userName: string;
  orgId: string;
  categoryId: string;
  messageCount: number;
  rating: number;
  ratingCount: number;
}
