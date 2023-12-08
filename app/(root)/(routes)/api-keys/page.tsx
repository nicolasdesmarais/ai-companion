import apiKeyService from "@/src/domain/services/ApiKeyService";
import { encodePermission } from "@/src/security/models/Permission";
import securityService from "@/src/security/services/SecurityService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";
import { APIKeysForm } from "./components/api-keys-form";

const ApiKeysPage = async () => {
  const authorizationContext = getUserAuthorizationContext();

  if (!authorizationContext?.orgId || !authorizationContext?.userId) {
    return redirectToSignIn();
  }

  const { orgId, userId, permissions } = authorizationContext;

  const apiKeys = await apiKeyService.getApiKeysByOrgIdAndUserId(orgId, userId);

  const availableApiPermissions = securityService
    .getAvailableApiPermissions(permissions)
    .map((permission) => encodePermission(permission))
    .sort();

  return (
    <APIKeysForm
      userScopes={availableApiPermissions}
      initialApiKeys={apiKeys}
    />
  );
};

export default ApiKeysPage;
