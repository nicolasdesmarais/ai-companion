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
  trainingDescription: string;
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
  data: AIDetailDto[];
}

export interface AISummaryDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  description: string;
  src: string;
  userId: string;
  userName: string;
  categoryId: string;

  // Only included based on profile settings
  modelId?: string;
  instructions?: string;
}

export interface AIDetailDto extends AISummaryDto {
  profile: AIProfile;
  messageCount: number;
  rating: number;
  ratingCount: number;

  // Only included based on profile settings
  options?: AIModelOptions;
  visibility?: AIVisibility;
}
