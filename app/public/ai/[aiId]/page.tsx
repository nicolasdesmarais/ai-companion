import { PublicAiChat } from "@/components/public-ai-chat";
import aiService from "@/src/domain/services/AIService";
import type { Metadata, ResolvingMetadata } from "next";

interface Props {
  params: {
    aiId: string;
  };
}
const PublicAiPage = async ({ params }: Props) => {
  return <PublicAiChat aiId={params.aiId} />;
};

export async function generateMetadata(
  { params }: Props,
  parent: ResolvingMetadata
): Promise<Metadata> {
  const ais = await aiService.findPublicAIs();
  const ai = ais.find((ai) => ai.id === params.aiId);

  if (ai) {
    return {
      title: `${ai.name} | AppDirect AI`,
      openGraph: {
        images: [
          {
            url: ai.src,
            width: 512,
            height: 512,
          },
        ],
        description: ai.description,
        url: `https://appdirect.ai/public/ai/${ai.id}`,
        siteName: "AppDirect AI",
        locale: "en_US",
        type: "website",
      },
    };
  } else
    return {
      title: "AppDirect AI",
    };
}

export default PublicAiPage;
