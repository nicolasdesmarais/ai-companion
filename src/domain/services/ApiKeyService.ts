import {
  CreateApiKeyRequest,
  CreateApiKeyResponse,
  ListApiKeyResponse,
  UpdateApiKeyRequest,
  UpdateApiKeyResponse,
} from "@/src/domain/ports/api/ApiKeysApi";
import prismadb from "@/src/lib/prismadb";
import { isValidScope } from "@/src/security/models/Permission";
import { JsonValue } from "@prisma/client/runtime/library";
import { createHash, randomBytes } from "crypto";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";

const apiKeySelect = {
  id: true,
  createdAt: true,
  updatedAt: true,
  lastUsedAt: true,
  orgId: true,
  userId: true,
  name: true,
  scopes: true,
};

export class ApiKeyService {
  /**
   * Retrieves an API key by its bearer token.
   * The bearer token is the API key hashed with SHA256.
   * @param bearerToken
   * @returns
   */
  public async getApiKeyFromBearerToken(bearerToken: string) {
    const hashedKey = this.hashApiKey(bearerToken);
    const apiKey = await prismadb.apiKey.findUnique({
      where: {
        key: hashedKey,
      },
    });

    if (!apiKey) {
      return null;
    }

    const scopes = this.getScopesFromJson(apiKey.scopes);
    return {
      ...apiKey,
      scopes,
    };
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
      select: apiKeySelect,
      where: {
        orgId,
        userId,
      },
    });

    const typedApiKeys = apiKeys.map((key) => {
      const scopes = this.getScopesFromJson(key.scopes);

      return {
        ...key,
        scopes,
      };
    });

    return typedApiKeys;
  }

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

    const validScopes = this.getValidScopes(request.scopes);

    const createdApiKey = await prismadb.apiKey.create({
      data: {
        orgId,
        userId,
        name: request.name,
        scopes: validScopes,
        key: hashedApiKey,
      },
    });

    return {
      ...createdApiKey,
      key: apiKey,
    };
  }

  private getScopesFromJson(scopesJson: JsonValue): string[] {
    const scopesArray =
      scopesJson != null && Array.isArray(scopesJson) ? scopesJson : [];

    return scopesArray.map((scope) => {
      if (scope !== null) {
        return scope as string;
      } else {
        throw new Error(`Unknown scope: ${scope}`);
      }
    });
  }

  public async updateApiKey(
    orgId: string,
    userId: string,
    apiKeyId: string,
    request: UpdateApiKeyRequest
  ): Promise<UpdateApiKeyResponse> {
    const apiKey = await prismadb.apiKey.findUnique({
      where: {
        id: apiKeyId,
      },
    });

    if (!apiKey) {
      throw new EntityNotFoundError(`No API key found with id ${apiKeyId}`);
    }

    if (apiKey.orgId !== orgId || apiKey.userId !== userId) {
      throw new ForbiddenError("Forbidden");
    }

    const validScopes = this.getValidScopes(request.scopes);

    const updatedKey = await prismadb.apiKey.update({
      select: apiKeySelect,
      where: {
        id: apiKeyId,
      },
      data: {
        name: request.name,
        scopes: validScopes,
      },
    });

    return updatedKey;
  }

  private getValidScopes(scopes: string[]): string[] {
    return scopes.filter(isValidScope);
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
