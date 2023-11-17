import apiKeyService from "@/src/domain/services/ApiKeyService";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { APIKeysForm } from "./components/api-keys-form";

const ApiKeysPage = async () => {
  const { orgId, userId } = await auth();
  if (!orgId || !userId) {
    return redirectToSignIn();
  }

  const apiKeys = await apiKeyService.getApiKeysByOrgIdAndUserId(orgId, userId);

  return <APIKeysForm initialApiKeys={apiKeys} />;
};

export default ApiKeysPage;
