import { AI } from "@prisma/client";
import { AuthorizationContext } from "../models/AuthorizationContext";
import { SecuredAction } from "../models/SecuredAction";
import { SecuredResourceAccessLevel } from "../models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "../models/SecuredResourceType";

export class AISecurityService {
  public static canUpdateAI(
    authorizationContext: AuthorizationContext,
    ai: AI
  ) {
    const { orgId, userId, permissions } = authorizationContext;

    for (const permission of permissions) {
      if (
        permission.resourceType === SecuredResourceType.AI &&
        permission.action === SecuredAction.WRITE
      ) {
        switch (permission.accessLevel) {
          case SecuredResourceAccessLevel.INSTANCE:
            return true;
          case SecuredResourceAccessLevel.ORGANIZATION:
            if (ai.orgId === orgId) {
              return true;
            }
          case SecuredResourceAccessLevel.SELF:
            if (ai.orgId === orgId && ai.userId === userId) {
              return true;
            }
            break;
        }
      }
    }
    return false;
  }

  public static canDeleteAI(
    authorizationContext: AuthorizationContext,
    ai: AI
  ) {
    return this.canUpdateAI(authorizationContext, ai);
  }
}
