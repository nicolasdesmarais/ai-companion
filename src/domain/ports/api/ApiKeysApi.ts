export interface CreateApiKeyRequest {
  name: string;
  scopes: string[];
}

export interface CreateApiKeyResponse {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  orgId: string;
  userId: string;
  key: string;
}

export interface UpdateApiKeyRequest {
  name: string;
  scopes: string[];
}

export interface UpdateApiKeyResponse {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  orgId: string;
  userId: string;
}

export interface ListApiKeyResponse {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  lastUsedAt: Date | null;
  name: string;
  orgId: string;
  userId: string;
  scopes: string[];
}
