import { auth } from "@clerk/nextjs";
import { headers } from "next/headers";
import apiKeyService from "../domain/services/ApiKeyService";
import {
  AuthorizationContext,
  AuthorizationContextType,
} from "../domain/types/AuthorizationContext";

export async function getAuthorizationContext(): Promise<AuthorizationContext | null> {
  const authn = await auth();
  if (authn?.userId && authn?.orgId) {
    return {
      orgId: authn.orgId,
      userId: authn.userId,
      type: AuthorizationContextType.USER,
    };
  }

  const headerPayload = headers();
  const authHeader = headerPayload.get("Authorization");
  if (authHeader && authHeader.startsWith("Bearer ")) {
    const token = authHeader.split(" ")[1];
    const verifyApiKey = await apiKeyService.getApiKeyFromBearerToken(token);
    if (verifyApiKey) {
      return {
        orgId: verifyApiKey.orgId,
        userId: verifyApiKey.userId,
        type: AuthorizationContextType.API,
      };
    }
  }

  return null;
}
