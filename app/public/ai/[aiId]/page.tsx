import { PublicAiChat } from "@/components/public-ai-chat";
import aiService from "@/src/domain/services/AIService";
import { aspectFill } from "@/src/lib/utils";
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

  const ai = await aiService.findPublicAIById(params.aiId);

  if (ai) {
    return {
      title: `${ai.name} | AppDirect AI`,
      metadataBase: new URL("https://appdirect.ai"),
      openGraph: {
        images: [
          {
            url: aspectFill(ai.src, "1.91") || ai.src,
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
