import { currentUser } from "@clerk/nextjs";
import { GoogleDriveForm } from "./components/google-drive-knowledge";

interface GoogleDriveKnowledgePageProps {
  params: {
    aiId: string;
  };
}

const GoogleDriveKnowledgePage = async ({
  params,
}: GoogleDriveKnowledgePageProps) => {
  const user = await currentUser();
  if (!user) {
    return;
  }

  return (
    <div className="container mx-auto mt-10">
      <h1 className="text-2xl mb-4">Search Files</h1>
      <GoogleDriveForm aiId={params.aiId} />
    </div>
  );
};

export default GoogleDriveKnowledgePage;
