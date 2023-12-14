import { AuthorizationContext } from "../models/AuthorizationContext";
import { SecuredAction } from "../models/SecuredAction";
import { SecuredResourceAccessLevel } from "../models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "../models/SecuredResourceType";

type BaseEntity = {
  orgId: string;
  userId: string;
};

export class BaseEntitySecurityService {
  public static hasPermission(
    authorizationContext: AuthorizationContext,
    resourceType: SecuredResourceType,
    action: SecuredAction,
    accessLevel: SecuredResourceAccessLevel
  ) {
    const { permissions } = authorizationContext;

    const hasPermission = permissions.some((permission) => {
      return (
        permission.resourceType === resourceType &&
        permission.action === action &&
        permission.accessLevel === accessLevel
      );
    });

    return hasPermission;
  }

  public static hasPermissionOnEntity(
    authorizationContext: AuthorizationContext,
    entity: BaseEntity,
    resourceType: SecuredResourceType,
    action: SecuredAction
  ) {
    const { orgId, userId, permissions } = authorizationContext;

    for (const permission of permissions) {
      if (
        permission.resourceType === resourceType &&
        permission.action === action
      ) {
        switch (permission.accessLevel) {
          case SecuredResourceAccessLevel.INSTANCE:
            return true;
          case SecuredResourceAccessLevel.ORGANIZATION:
            if (entity.orgId === orgId) {
              return true;
            }
          case SecuredResourceAccessLevel.SELF:
            if (entity.orgId === orgId && entity.userId === userId) {
              return true;
            }
            break;
        }
      }
    }
    return false;
  }
}
