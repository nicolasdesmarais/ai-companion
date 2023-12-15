import { AIVisibility } from "@prisma/client";
import { AIModelOptions } from "./AIModel";

export interface AISummaryDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  description: string;
  src: string;
  orgId: string;
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

  // Only included based on access and profile settings
  groups?: string[];
  options?: AIModelOptions;
  visibility?: AIVisibility;
}

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