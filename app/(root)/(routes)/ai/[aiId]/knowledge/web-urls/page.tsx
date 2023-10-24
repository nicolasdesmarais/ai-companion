import aiService from "@/src/domain/services/AIService";
import { currentUser, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import { WebUrlsForm } from "./components/web-urls-knowledge-form";

interface WebUrlsKnowledgePageProps {
  params: {
    aiId: string;
  };
}

const WebUrlsKnowledgePage = async ({ params }: WebUrlsKnowledgePageProps) => {
  const user = await currentUser();
  if (!user) {
    return redirectToSignIn();
  }

  const ai = await aiService.findAIById(params.aiId);
  if (!ai) {
    return redirect("/404");
  }

  return (
    <div className="container mx-auto mt-10">
      <h1 className="text-2xl mb-4">Create Data Store</h1>
      <WebUrlsForm aiId={params.aiId} />
    </div>
  );
};

export default WebUrlsKnowledgePage;
