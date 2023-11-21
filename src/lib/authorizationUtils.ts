import { auth } from "@clerk/nextjs";
import { headers } from "next/headers";
import apiKeyService from "../domain/services/ApiKeyService";
import {
  AuthorizationContext,
  AuthorizationContextType,
  AuthorizationScope,
} from "../domain/types/AuthorizationContext";

const AUTHORIZATION_HEADER = "X-Authorization";

export async function getAuthorizationContext(): Promise<AuthorizationContext | null> {
  const authn = await auth();
  if (authn?.userId && authn?.orgId) {
    return {
      orgId: authn.orgId,
      userId: authn.userId,
      type: AuthorizationContextType.USER,
      scopes: Object.values(AuthorizationScope),
    };
  }

  const headerPayload = headers();
  const authHeader = headerPayload.get(AUTHORIZATION_HEADER);
  if (authHeader && authHeader.startsWith("Bearer ")) {
    const token = authHeader.split(" ")[1];
    const verifiedApiKey = await apiKeyService.getApiKeyFromBearerToken(token);
    if (verifiedApiKey) {
      return {
        orgId: verifiedApiKey.orgId,
        userId: verifiedApiKey.userId,
        type: AuthorizationContextType.API,
        scopes: verifiedApiKey.scopes,
      };
    }
  }

  return null;
}
