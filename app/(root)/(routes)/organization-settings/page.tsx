import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import { OrganizationSettingsForm } from "./components/organization-settings-form";

const OrganizationSettingsPage = async () => {
  const authorizationContext = getUserAuthorizationContext();

  if (!authorizationContext?.orgId || !authorizationContext?.userId) {
    return redirectToSignIn();
  }

  const { permissions } = authorizationContext;
  const hasAccess = permissions.some(
    (permission) =>
      permission.resourceType === SecuredResourceType.ORG_SETTINGS &&
      permission.accessLevel === SecuredResourceAccessLevel.ORGANIZATION &&
      permission.action === SecuredAction.WRITE
  );
  if (!hasAccess) {
    return redirect("/");
  }

  return <OrganizationSettingsForm />;
};

export default OrganizationSettingsPage;
