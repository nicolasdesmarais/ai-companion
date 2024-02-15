import { PublicAiChat } from "@/components/public-ai-chat";

interface Props {
  params: {
    aiId: string;
  };
}
const PublicAiPage = async ({ params }: Props) => {
  return <PublicAiChat aiId={params.aiId} />;
};

export default PublicAiPage;
