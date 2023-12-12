import { AIEditor } from "@/components/ai-editor";
import { GroupModal } from "@/components/group-modal";
import aiModelService from "@/src/domain/services/AIModelService";
import aiService from "@/src/domain/services/AIService";
import groupService from "@/src/domain/services/GroupService";
import prismadb from "@/src/lib/prismadb";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { redirectToSignIn } from "@clerk/nextjs";

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

  const { userId } = authorizationContext;

  const initialAi = await aiService.findAIForUser(
    authorizationContext,
    params.aiId
  );

  const models = await aiModelService.getAIModels();

  const categories = await prismadb.category.findMany();

  const groups = await groupService.findGroupsByUser(authorizationContext);

  return (
    <>
      <AIEditor
        initialAi={initialAi}
        aiModels={models}
        categories={categories}
        groups={groups}
      />
      <GroupModal />
    </>
  );
};

export default AIIdPage;
