import { AIEditor } from "@/components/ai-editor";
import prismadb from "@/src/lib/prismadb";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import groupService from "@/src/domain/services/GroupService";
import { GroupModal } from "@/components/group-modal";

interface AIIdPageProps {
  params: {
    aiId: string;
  };
}

const AIIdPage = async ({ params }: AIIdPageProps) => {
  const { userId, orgId } = await auth();

  if (!userId) {
    return redirectToSignIn();
  }

  const initialAi = await prismadb.aI.findUnique({
    where: {
      id: params.aiId,
      userId,
    },
    include: {
      dataSources: {
        include: {
          dataSource: true,
        },
      },
    },
  });

  const categories = await prismadb.category.findMany();

  const groups = await groupService.findGroupsByUser(orgId, userId);

  return (
    <>
      <AIEditor initialAi={initialAi} categories={categories} groups={groups} />
      <GroupModal />
    </>
  );
};

export default AIIdPage;
