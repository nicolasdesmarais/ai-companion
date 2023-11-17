export interface CreateApiKeyRequest {
  name: string;
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
