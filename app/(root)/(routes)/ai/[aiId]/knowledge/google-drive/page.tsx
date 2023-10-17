import { OAuthTokenService } from "@/domain/services/OAuthTokenService";
import { currentUser } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
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

  const oauthTokenService = new OAuthTokenService();
  const hasOAuthToken = await oauthTokenService.hasOAuthToken(
    OAuthTokenProvider.GOOGLE,
    user.id
  );

  return (
    <div className="container mx-auto mt-10">
      <h1 className="text-2xl mb-4">Add Google Drive Folder</h1>
      <GoogleDriveForm aiId={params.aiId} hasOAuthToken={hasOAuthToken} />
    </div>
  );
};

export default GoogleDriveKnowledgePage;
