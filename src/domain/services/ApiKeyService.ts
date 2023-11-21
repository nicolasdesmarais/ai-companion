import prismadb from "@/src/lib/prismadb";
import {
  ApiScope,
  CreateApiKeyRequest,
  CreateApiKeyResponse,
  ListApiKeyResponse,
} from "@/src/ports/api/ApiKeysApi";
import { createHash, randomBytes } from "crypto";

export class ApiKeyService {
  /**
   * Creates an API key for the given user and organization.
   */
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
        scopes: request.scopes,
        key: hashedApiKey,
      },
    });

    return {
      ...createdApiKey,
      key: apiKey,
    };
  }

  /**
   * Retrieves an API key by its bearer token.
   * The bearer token is the API key hashed with SHA256.
   * @param bearerToken
   * @returns
   */
  public async getApiKeyFromBearerToken(bearerToken: string) {
    const hashedKey = this.hashApiKey(bearerToken);
    return await prismadb.apiKey.findUnique({
      where: {
        key: hashedKey,
      },
    });
  }

  /**
   * Returns a list of all API keys for the given organization and user.
   * @param orgId
   * @param userId
   * @returns
   */
  public async getApiKeysByOrgIdAndUserId(
    orgId: string,
    userId: string
  ): Promise<ListApiKeyResponse[]> {
    const apiKeys = await prismadb.apiKey.findMany({
      select: {
        id: true,
        createdAt: true,
        updatedAt: true,
        lastUsedAt: true,
        orgId: true,
        userId: true,
        name: true,
        scopes: true,
      },
      where: {
        orgId,
        userId,
      },
    });

    const typedApiKeys = apiKeys.map((key) => {
      // Ensure that scopes is an array
      const scopesArray =
        key.scopes != null && Array.isArray(key.scopes) ? key.scopes : [];

      // Now we can safely map the scopes, casting each one to ApiScope if it matches
      const scopes = scopesArray.map((scope) => {
        if (scope !== null && Object.values(ApiScope).includes(scope as any)) {
          return scope as ApiScope;
        } else {
          throw new Error(`Unknown scope: ${scope}`);
        }
      });

      return {
        ...key,
        scopes,
      };
    });

    return typedApiKeys;
  }

  public async deleteApiKey(orgId: string, userId: string, apiKeyId: string) {
    await prismadb.apiKey.delete({
      where: {
        id: apiKeyId,
        orgId,
        userId,
      },
    });
  }

  private generateApiKey(): string {
    return "sk-" + randomBytes(24).toString("hex");
  }

  private hashApiKey(apiKey: string): string {
    return createHash("sha256").update(apiKey).digest("hex");
  }
}

const apiKeyService = new ApiKeyService();
export default apiKeyService;
