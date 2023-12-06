import { Permission } from "./Permission";

export interface AuthorizationContext {
  orgId: string;
  userId: string;
  type: AuthorizationContextType;
  scopes: AuthorizationScope[];
  permissions?: Permission[];
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
  ORG_CLIENT_CREDENTIALS_WRITE = "orgClientCredentials.write",
}

export const visibleAuthorizationScopes = [
  AuthorizationScope.CHATS_READ,
  AuthorizationScope.CHATS_WRITE,
  AuthorizationScope.DATA_SOURCES_READ,
  AuthorizationScope.DATA_SOURCES_WRITE,
];
