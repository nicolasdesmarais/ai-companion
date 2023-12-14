import { AIEditor } from "@/components/ai-editor";
import { GroupModal } from "@/components/group-modal";
import aiModelService from "@/src/domain/services/AIModelService";
import aiService from "@/src/domain/services/AIService";
import groupService from "@/src/domain/services/GroupService";
import prismadb from "@/src/lib/prismadb";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

export const maxDuration = 300;

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

  const initialAi = await aiService.findAIForUser(
    authorizationContext,
    params.aiId
  );

  if (initialAi === null) {
    return redirect("/");
  }

  const models = await aiModelService.getAIModels();

  const categories = await prismadb.category.findMany();

  const groups = await groupService.findGroupsByUser(authorizationContext);

  const hasInstanceAccess = BaseEntitySecurityService.hasPermission(
    authorizationContext,
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    SecuredResourceAccessLevel.INSTANCE
  );

  return (
    <>
      <AIEditor
        initialAi={initialAi}
        aiModels={models}
        categories={categories}
        groups={groups}
        hasInstanceAccess={hasInstanceAccess}
      />
      <GroupModal />
    </>
  );
};

export default AIIdPage;
