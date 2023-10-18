import { auth, redirectToSignIn } from "@clerk/nextjs";

import prismadb from "@/lib/prismadb";

import { AIForm } from "./components/ai-form";

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

  const companion = await prismadb.companion.findUnique({
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

  return <AIForm initialData={companion} categories={categories} />;
};

export default AIIdPage;
