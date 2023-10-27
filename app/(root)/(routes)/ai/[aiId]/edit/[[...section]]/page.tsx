import { AIEditor } from "@/components/ai-editor";
import prismadb from "@/src/lib/prismadb";
import { auth, redirectToSignIn } from "@clerk/nextjs";

interface AIIdPageProps {
  params: {
    aiId: string;
  };
}

const AIIdPage = async ({ params }: AIIdPageProps) => {
  const { userId } = auth();

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

  return <AIEditor initialAi={initialAi} categories={categories} />;
};

export default AIIdPage;
