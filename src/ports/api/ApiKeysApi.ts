export interface CreateApiKeyRequest {
  name: string;
  scopes: ApiScope[];
}

export enum ApiScope {
  CHATS_READ = "chats.read",
  CHATS_WRITE = "chats.write",
  DATA_SOURCES_READ = "dataSources.read",
  DATA_SOURCES_WRITE = "dataSources.write",
}

export interface CreateApiKeyResponse {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  userId: string;
  key: string;
}

export interface ListApiKeyResponse {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  lastUsedAt: Date | null;
  name: string;
  orgId: string;
  userId: string;
}
