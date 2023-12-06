import { SecuredAction } from "./SecuredAction";
import { SecuredResourceAccessLevel } from "./SecuredResourceAccessLevel";
import { SecuredResourceType } from "./SecuredResourceType";
import { SecuredRole } from "./SecuredRoles";

export type Permission = {
  resourceType: SecuredResourceType;
  action: SecuredAction;
  accessLevel: SecuredResourceAccessLevel;
};

export const encodePermission = (permission: Permission) => {
  return `${permission.resourceType}.${permission.action}.${permission.accessLevel}`;
};

export const rolePermissions: Record<SecuredRole, Permission[]> = {
  [SecuredRole.SUPERUSER]: Object.values(SecuredResourceType).flatMap(
    (resourceType) =>
      Object.values(SecuredAction).map((action) => ({
        resourceType,
        action,
        accessLevel: SecuredResourceAccessLevel.INSTANCE,
      }))
  ),
  [SecuredRole.ORG_ADMIN]: Object.values(SecuredResourceType).flatMap(
    (resourceType) =>
      Object.values(SecuredAction).map((action) => ({
        resourceType,
        action,
        accessLevel: SecuredResourceAccessLevel.ORGANIZATION,
      }))
  ),
  [SecuredRole.USER]: Object.values(SecuredResourceType).flatMap(
    (resourceType) =>
      Object.values(SecuredAction).map((action) => ({
        resourceType,
        action,
        accessLevel: SecuredResourceAccessLevel.SELF,
      }))
  ),
};
