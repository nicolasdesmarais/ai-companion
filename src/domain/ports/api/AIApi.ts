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
