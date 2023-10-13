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
  PUBLIC = "PUBLIC", // Only return AIs that are public,
  ALL = "ALL", // Return all AIs that the user has access to
}
