import { SuperPinecone } from "@/components/super-pinecone";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

const SuperPineconePage = async () => {
  const { userId, sessionClaims } = await auth();

  if (!userId) {
    return redirectToSignIn();
  }

  if (!(sessionClaims?.meta as any)?.superuser) {
    return redirect("/");
  }
  if (!process.env.PINECONE_SERVERLESS_INDEX) {
    return <div>Set PINECONE_INDEX</div>;
  }

  return <SuperPinecone />;
};

export default SuperPineconePage;
