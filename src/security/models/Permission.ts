import { SecuredAction } from "./SecuredAction";
import { SecuredResourceAccessLevel } from "./SecuredResourceAccessLevel";
import {
  SecuredResourceType,
  orgOnlyResourceTypes,
} from "./SecuredResourceType";
import { SecuredRole } from "./SecuredRoles";

export type Permission = {
  resourceType: SecuredResourceType;
  action: SecuredAction;
  accessLevel: SecuredResourceAccessLevel;
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
  [SecuredRole.USER]: Object.values(SecuredResourceType)
    .filter((resourceType) => orgOnlyResourceTypes.includes(resourceType))
    .flatMap((resourceType) =>
      Object.values(SecuredAction).map((action) => ({
        resourceType,
        action,
        accessLevel: SecuredResourceAccessLevel.SELF,
      }))
    ),
};

export const encodePermission = (permission: Permission) => {
  return `${permission.resourceType}.${permission.action}.${permission.accessLevel}`;
};

export const decodePermission = (permission: string): Permission => {
  const [resourceType, action, accessLevel] = permission.split(".");
  return {
    resourceType: resourceType as any,
    action: action as any,
    accessLevel: accessLevel as any,
  };
};

export const isValidScope = (scope: string): boolean => {
  const { resourceType, action, accessLevel } = decodePermission(scope);
  return (
    Object.values(SecuredResourceType).includes(resourceType) &&
    Object.values(SecuredAction).includes(action) &&
    Object.values(SecuredResourceAccessLevel).includes(accessLevel)
  );
};
