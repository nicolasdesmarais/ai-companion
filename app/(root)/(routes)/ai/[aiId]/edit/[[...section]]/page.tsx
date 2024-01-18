import { AIEditor } from "@/components/ai-editor";
import { GroupModal } from "@/components/group-modal";
import aiModelService from "@/src/domain/services/AIModelService";
import aiService from "@/src/domain/services/AIService";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";
import { GroupSecurityService } from "@/src/security/services/GroupSecurityService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import { cache } from "react";

export const maxDuration = 300;
export const revalidate = 3600;

interface AIIdPageProps {
  params: {
    aiId: string;
  };
}

const AIIdPage = async ({ params }: AIIdPageProps) => {
  const authorizationContext = getUserAuthorizationContext();
  if (!authorizationContext) {
    return redirectToSignIn();
  }

  let initialAi = null;
  if (params.aiId !== "new") {
    initialAi = await aiService.findAIForUser(
      authorizationContext,
      params.aiId
    );

    if (initialAi === null) {
      return redirect("/");
    }
  }

  const models = await cache(() => aiModelService.getAIModels())();

  const hasInstanceAccess = BaseEntitySecurityService.hasPermission(
    authorizationContext,
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    SecuredResourceAccessLevel.INSTANCE
  );

  const hasElevatedWriteAccess =
    GroupSecurityService.hasElevatedWriteAccess(authorizationContext);

  return (
    <>
      <AIEditor
        initialAi={initialAi}
        aiModels={models}
        hasInstanceAccess={hasInstanceAccess}
      />
      <GroupModal hasElevatedWriteAccess={hasElevatedWriteAccess} />
    </>
  );
};

export default AIIdPage;
