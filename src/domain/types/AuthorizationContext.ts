export interface AuthorizationContext {
  orgId: string;
  userId: string;
  type: AuthorizationContextType;
  scopes: AuthorizationScope[];
}

export enum AuthorizationContextType {
  USER = "USER",
  API = "API",
}

export enum AuthorizationScope {
  CHATS_READ = "chats.read",
  CHATS_WRITE = "chats.write",
  DATA_SOURCES_READ = "dataSources.read",
  DATA_SOURCES_WRITE = "dataSources.write",
}
