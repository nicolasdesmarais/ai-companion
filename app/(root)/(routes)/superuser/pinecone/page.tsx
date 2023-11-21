import { SuperPinecone } from "@/components/super-pinecone";
import { isSuperuser } from "@/src/lib/utils";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

const SuperPineconePage = async () => {
  const { userId, user } = await auth();

  if (!userId) {
    return redirectToSignIn();
  }

  if (!isSuperuser(userId)) {
    console.log("Superuser attempt", userId);
    return redirect("/");
  }
  if (!process.env.PINECONE_INDEX) {
    return <div>Set PINECONE_INDEX</div>;
  }

  return <SuperPinecone />;
};

export default SuperPineconePage;
