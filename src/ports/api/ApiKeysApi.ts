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
