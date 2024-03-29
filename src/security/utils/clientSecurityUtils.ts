import {
  AuthorizationContext,
  AuthorizationContextType,
} from "../models/AuthorizationContext";
import { Permission } from "../models/Permission";

import { auth } from "@clerk/nextjs";
import { rolePermissions } from "../models/Permission";
import { SecuredRole } from "../models/SecuredRoles";

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
