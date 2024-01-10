import { AIEditor } from "@/components/ai-editor";

export const maxDuration = 300;

interface AIIdPageProps {
  params: {
    aiId: string;
  };
}

const AIIdPage = async ({ params }: AIIdPageProps) => {
  return (
    <>
      <AIEditor
        initialAi={null}
        aiModels={[]}
        categories={[]}
        groups={[]}
        hasInstanceAccess={true}
      />
    </>
  );
};

export default AIIdPage;
