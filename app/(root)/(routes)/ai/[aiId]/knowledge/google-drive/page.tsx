import { AIService } from "@/domain/services/AIService";
import { OAuthTokenService } from "@/domain/services/OAuthTokenService";
import { currentUser, redirectToSignIn } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { redirect } from "next/navigation";
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
    return redirectToSignIn();
  }

  const aiService = new AIService();
  const ai = await aiService.findAIById(params.aiId);
  if (!ai) {
    return redirect("/404");
  }

  const oauthTokenService = new OAuthTokenService();
  const oauthTokenEmails = await oauthTokenService.getOAuthTokenEmails(
    OAuthTokenProvider.GOOGLE,
    user.id
  );

  return (
    <div className="container mx-auto mt-10">
      <h1 className="text-2xl mb-4">Create Data Store</h1>
      <GoogleDriveForm aiId={params.aiId} oauthTokenEmails={oauthTokenEmails} />
    </div>
  );
};

export default GoogleDriveKnowledgePage;
