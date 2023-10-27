import { auth, redirectToSignIn } from "@clerk/nextjs";
import prismadb from "@/src/lib/prismadb";
import { AIEditor } from "@/components/ai-editor";

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

  const initialAi = await prismadb.companion.findUnique({
    where: {
      id: params.aiId,
      userId,
    },
    include: {
      knowledge: {
        include: {
          knowledge: true,
        },
      },
    },
  });

  const categories = await prismadb.category.findMany();

  return <AIEditor initialAi={initialAi} categories={categories} />;
};

export default AIIdPage;
