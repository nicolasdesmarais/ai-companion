import { AIDetailDto, AIProfile } from "@/src/domain/models/AI";
import { AIModelOptions } from "@/src/domain/models/AIModel";
import { AIVisibility } from "@prisma/client";

export interface ListAIsResponse {
  data: AIDetailDto[];
}

export interface ListAIsRequestParams {
  scope?: ListAIsRequestScope | null;
  groupId?: string | null;
  categoryId?: string | null;
  search?: string | null;
}

export enum ListAIsRequestScope {
  PRIVATE = "PRIVATE", // Only return AIs owned by the user and with private visibility
  OWNED = "OWNED", // Only return AIs owned by the user
  GROUP = "GROUP", // Only return AIs that are within the user's groups
  SHARED = "SHARED", // Only return AIs that are explicitly shared with the user
  ORGANIZATION = "ORGANIZATION", // Only return AIs that are shared within the user's organization
  PUBLIC = "PUBLIC", // Only return AIs that are public,
  ALL = "ALL", // Return all AIs that the user has access to
  INSTANCE = "INSTANCE",
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
export interface ShareAIRequest {
  emails: string;
}

export interface CreateAIDataSourceRequest {
  dataSourceId: string;
}

export interface CreateAIDataSourceResponse {
  aiId: string;
  dataSourceId: string;
}
