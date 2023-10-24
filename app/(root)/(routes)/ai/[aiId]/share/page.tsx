import { AIService } from "@/src/domain/services/AIService";
import { auth } from "@clerk/nextjs";
import { ShareAIForm } from "./components/share-ai-form";

interface ShareAIPageProps {
  params: {
    aiId: string;
  };
}

const ShareAIPage = async ({ params }: ShareAIPageProps) => {
  const authentication = await auth();

  if (!authentication?.userId) {
    return;
  }

  const aiService = new AIService();
  aiService.findAIById(params.aiId);
  const ai = await aiService.findAIById(params.aiId);
  if (!ai) {
    return;
  }

  return <ShareAIForm ai={ai} />;
};

export default ShareAIPage;
