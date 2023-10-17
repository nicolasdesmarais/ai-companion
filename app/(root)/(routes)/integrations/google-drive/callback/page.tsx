import { redirect } from "next/navigation";

interface GoogleCallbackPageProps {
  searchParams: {
    code: string;
  };
}

const GoogleCallbackPage = async ({
  searchParams,
}: GoogleCallbackPageProps) => {
  const code = searchParams.code;
  redirect(`/api/v1/integrations/google-drive/callback?code=${code}`);
};

export default GoogleCallbackPage;
