import { headers } from "next/headers";
import apiKeyService from "../../domain/services/ApiKeyService";
import {
  AuthorizationContext,
  AuthorizationContextType,
} from "../models/AuthorizationContext";
import { Permission, decodePermission } from "../models/Permission";

import { auth } from "@clerk/nextjs";
import { rolePermissions } from "../models/Permission";
import { SecuredRole } from "../models/SecuredRoles";

const AUTHORIZATION_HEADER = "X-Authorization";

export async function getAuthorizationContext(): Promise<AuthorizationContext | null> {
  return getUserAuthorizationContext() || (await getApiAuthorizationContext());
}

export function getUserAuthorizationContext(): AuthorizationContext | null {
  const authn = auth();
  if (!authn?.userId || !authn?.orgId) {
    return null;
  }

  // Add permissions based on the user's role
  const permissions: Permission[] = [];
  if ((authn.sessionClaims?.meta as any)?.superuser) {
    permissions.push(...rolePermissions[SecuredRole.SUPERUSER]);
  }
  if (authn.orgRole === "admin") {
    permissions.push(...rolePermissions[SecuredRole.ORG_ADMIN]);
  }

  permissions.push(...rolePermissions[SecuredRole.USER]);

  return {
    orgId: authn.orgId,
    userId: authn.userId,
    type: AuthorizationContextType.USER,
    permissions,
  };
}

export async function getApiAuthorizationContext(): Promise<AuthorizationContext | null> {
  const headerPayload = headers();
  const authHeader = headerPayload.get(AUTHORIZATION_HEADER);
  if (!authHeader?.startsWith("Bearer ")) {
    return null;
  }

  const token = authHeader.split(" ")[1];
  const verifiedApiKey = await apiKeyService.getApiKeyFromBearerToken(token);
  if (verifiedApiKey) {
    const permissions: Permission[] = verifiedApiKey.scopes.map((scope) =>
      decodePermission(scope)
    );

    return {
      orgId: verifiedApiKey.orgId,
      userId: verifiedApiKey.userId,
      type: AuthorizationContextType.API,
      permissions,
    };
  }

  return null;
}
