import { AuthorizationScope } from "../security/models/AuthorizationContext";
import { getAuthorizationContext } from "../security/utils/authorizationUtils";

export const withAuthorization = (
  requiredScope: AuthorizationScope,
  handler: Function
) => {
  return async (request: Request, context: any) => {
    const authorizationContext = await getAuthorizationContext();

    if (!authorizationContext?.orgId || !authorizationContext?.userId) {
      return new Response("Unauthorized", { status: 401 });
    }

    const { orgId, userId, scopes } = authorizationContext;

    const hasRequiredScopes = scopes.includes(requiredScope);

    if (!hasRequiredScopes) {
      return new Response("Forbidden", { status: 403 });
    }

    context.orgId = orgId;
    context.userId = userId;

    return handler(request, context);
  };
};
