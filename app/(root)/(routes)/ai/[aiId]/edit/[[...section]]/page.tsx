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
let start, end;
const AIIdPage = async ({ params }: AIIdPageProps) => {
  start = performance.now();
  const authorizationContext = getUserAuthorizationContext();
  end = performance.now();
  console.log(`getUserAuthorizationContext time: ${end - start} ms`);
  if (!authorizationContext) {
    return redirectToSignIn();
  }

  let initialAi = null;
  if (params.aiId !== "new") {
    start = performance.now();
    initialAi = await aiService.findAIForUser(
      authorizationContext,
      params.aiId
    );
    end = performance.now();
    console.log(`findAIForUser time: ${end - start} ms`);

    if (initialAi === null) {
      return redirect("/");
    }
  }

  start = performance.now();
  const models = await aiModelService.getAIModels();
  end = performance.now();
  console.log(`getAIModels time: ${end - start} ms`);

  start = performance.now();
  //TODO: optimize
  const categories = await prismadb.category.findMany();
  end = performance.now();
  console.log(`category time: ${end - start} ms`);

  start = performance.now();
  const groups = await groupService.findGroupsByUser(authorizationContext);
  end = performance.now();
  console.log(`findGroupsByUser time: ${end - start} ms`);

  start = performance.now();
  const hasInstanceAccess = BaseEntitySecurityService.hasPermission(
    authorizationContext,
    SecuredResourceType.AI,
    SecuredAction.WRITE,
    SecuredResourceAccessLevel.INSTANCE
  );
  end = performance.now();
  console.log(`hasPermission time: ${end - start} ms`);

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
