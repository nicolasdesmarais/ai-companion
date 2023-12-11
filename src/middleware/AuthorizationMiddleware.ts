import { SecuredAction } from "../security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "../security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "../security/models/SecuredResourceType";
import { getAuthorizationContext } from "../security/utils/securityUtils";

export const withAuthorization = (
  resourceType: SecuredResourceType,
  action: SecuredAction,
  allowedAccessLevels: SecuredResourceAccessLevel[],
  handler: Function
) => {
  return async (request: Request, context: any) => {
    const authorizationContext = await getAuthorizationContext();

    if (!authorizationContext?.orgId || !authorizationContext?.userId) {
      return new Response("Unauthorized", { status: 401 });
    }

    const { orgId, userId, permissions } = authorizationContext;
    const hasRequiredPermission = permissions.some((permission) => {
      return (
        permission.resourceType === resourceType &&
        permission.action === action &&
        allowedAccessLevels.includes(permission.accessLevel)
      );
    });

    if (!hasRequiredPermission) {
      return new Response("Forbidden", { status: 403 });
    }

    context.orgId = orgId;
    context.userId = userId;
    context.authorizationContext = authorizationContext;

    return handler(request, context);
  };
};
