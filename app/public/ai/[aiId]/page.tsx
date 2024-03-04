import { PublicAiChat } from "@/components/public-ai-chat";
import aiService from "@/src/domain/services/AIService";
import { auth } from "@clerk/nextjs";
import type { Metadata, ResolvingMetadata } from "next";
import { redirect } from "next/navigation";

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
  const { userId } = auth();

  if (userId) {
    return redirect(`/ai/${params.aiId}`);
  }

  const ais = await aiService.findPublicAIs();
  const ai = ais.find((ai) => ai.id === params.aiId);

  if (ai) {
    return {
      title: `${ai.name} | AppDirect AI`,
      openGraph: {
        images: [
          {
            url: ai.src,
            width: 256,
            height: 256,
          },
        ],
        description: ai.description,
        url: `./public/ai/${ai.id}`,
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
