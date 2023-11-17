import prismadb from "@/src/lib/prismadb";
import {
  CreateApiKeyRequest,
  CreateApiKeyResponse,
} from "@/src/ports/api/ApiKeysApi";
import { createHash, randomBytes } from "crypto";

export class ApiKeyService {
  public async createApiKey(
    orgId: string,
    userId: string,
    request: CreateApiKeyRequest
  ): Promise<CreateApiKeyResponse> {
    const apiKey = this.generateApiKey();
    const hashedApiKey = this.hashApiKey(apiKey);

    const createdApiKey = await prismadb.apiKey.create({
      data: {
        orgId,
        userId,
        name: request.name,
        key: hashedApiKey,
      },
    });

    return {
      ...createdApiKey,
      key: apiKey,
    };
  }

  public async getApiKeyFromBearerToken(bearerToken: string) {
    const hashedKey = this.hashApiKey(bearerToken);
    return await prismadb.apiKey.findUnique({
      where: {
        key: hashedKey,
      },
    });
  }

  private generateApiKey(): string {
    return randomBytes(32).toString("hex");
  }

  private hashApiKey(apiKey: string): string {
    return createHash("sha256").update(apiKey).digest("hex");
  }
}

const apiKeyService = new ApiKeyService();
export default apiKeyService;
