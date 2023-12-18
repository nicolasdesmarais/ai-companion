import orgClientCredentialsService from "@/src/domain/services/OrgClientCredentialsService";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { redirect } from "next/navigation";
import { OrganizationSettingsForm } from "./components/organization-settings-form";

const OrganizationSettingsPage = async () => {
  const authorizationContext = getUserAuthorizationContext();

  if (!authorizationContext?.orgId || !authorizationContext?.userId) {
    return redirectToSignIn();
  }

  const { orgId, permissions } = authorizationContext;
  const hasAccess = permissions.some(
    (permission) =>
      permission.resourceType === SecuredResourceType.ORG_SETTINGS &&
      permission.accessLevel === SecuredResourceAccessLevel.ORGANIZATION &&
      permission.action === SecuredAction.WRITE
  );
  if (!hasAccess) {
    return redirect("/");
  }

  const googleDriveCredentials =
    await orgClientCredentialsService.getOrgClientCredentialData(
      orgId,
      OAuthTokenProvider.GOOGLE
    );

  return <OrganizationSettingsForm data={googleDriveCredentials} />;
};

export default OrganizationSettingsPage;
