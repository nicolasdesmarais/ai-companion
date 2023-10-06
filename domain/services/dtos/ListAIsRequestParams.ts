export interface ListAIsRequestParams {
    workspaceIds?: string[] | null;
    categories?: string[] | null;
    search?: string | null;
    scope?: ListAIsRequestScope | null;
}

export enum ListAIsRequestScope {
    PRIVATE = "PRIVATE", // Only return AIs owned by the user and with private visibility
    OWNED = "OWNED", // Only return AIs owned by the user
    WORKSPACE = "WORKSPACE", // Only return AIs that are within the user's workspaces
    SHARED = "SHARED", // Only return AIs that are explicitly shared with the user
    PUBLIC = "PUBLIC", // Only return AIs that are public,
    ALL = "ALL" // Return all AIs that the user has access to
}