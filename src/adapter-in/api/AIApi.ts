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
  sort?: string | null;
}

export enum ListAIsRequestScope {
  PRIVATE = "PRIVATE", // Only return AIs owned by the user and with private visibility
  OWNED = "OWNED", // Only return AIs owned by the user
  GROUP = "GROUP", // Only return AIs that are within the user's groups
  SHARED = "SHARED", // Only return AIs that are explicitly shared with the user
  ORGANIZATION = "ORGANIZATION", // Only return AIs that are shared within the user's organization
  PUBLIC = "PUBLIC", // Only return AIs that are public,
  ALL = "ALL", // Return all AIs that the user has access to
  INSTANCE = "INSTANCE", // No filters
  INSTANCE_ORGANIZATION = "INSTANCE_ORGANIZATION", // All AIs with org and group visibility
  INSTANCE_PRIVATE = "INSTANCE_PRIVATE", // All private AIs
  INSTANCE_NOT_VISIBLE = "INSTANCE_NOT_VISIBLE", // All AIs user would not see normally
  ADMIN = "ADMIN", // All AIs in the organization
  ADMIN_ORGANIZATION = "ADMIN_ORGANIZATION", // All AIs in the organization
  ADMIN_PRIVATE = "ADMIN_PRIVATE", // All AIs in the organization
  ADMIN_NOT_VISIBLE = "ADMIN_NOT_VISIBLE", // All AIs in the organization
}

export const SuperuserScopes = [
  ListAIsRequestScope.INSTANCE,
  ListAIsRequestScope.INSTANCE_ORGANIZATION,
  ListAIsRequestScope.INSTANCE_PRIVATE,
  ListAIsRequestScope.INSTANCE_NOT_VISIBLE,
];

export const AdminScopes = [
  ListAIsRequestScope.ADMIN,
  ListAIsRequestScope.ADMIN_ORGANIZATION,
  ListAIsRequestScope.ADMIN_PRIVATE,
  ListAIsRequestScope.ADMIN_NOT_VISIBLE,
];

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
