import { auth } from "@clerk/nextjs";
import { headers } from "next/headers";
import apiKeyService from "../../domain/services/ApiKeyService";
import {
  AuthorizationContext,
  AuthorizationContextType,
  AuthorizationScope,
} from "../models/AuthorizationContext";
import { Permission, rolePermissions } from "../models/Permission";
import { SecuredRole } from "../models/SecuredRoles";

const AUTHORIZATION_HEADER = "X-Authorization";

export async function getAuthorizationContext(): Promise<AuthorizationContext | null> {
  const authn = auth();

  if (authn?.userId && authn?.orgId) {
    const permissions: Permission[] = [];
    // Add permissions based on the user's role
    if ((authn.sessionClaims?.meta as any)?.superuser) {
      permissions.push(...rolePermissions[SecuredRole.SUPERUSER]);
    }
    if (authn.orgRole === "admin") {
      permissions.push(...rolePermissions[SecuredRole.ORG_ADMIN]);
    }

    return {
      orgId: authn.orgId,
      userId: authn.userId,
      type: AuthorizationContextType.USER,
      scopes: Object.values(AuthorizationScope),
      permissions,
    };
  }

  const headerPayload = headers();
  const authHeader = headerPayload.get(AUTHORIZATION_HEADER);
  if (authHeader?.startsWith("Bearer ")) {
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
