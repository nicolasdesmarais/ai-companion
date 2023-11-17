import { auth, redirectToSignIn } from "@clerk/nextjs";
import APIKeysForm from "./components/api-keys-form";

const ApiKeysPage = async () => {
  const { orgId, userId } = await auth();
  if (!orgId || !userId) {
    return redirectToSignIn();
  }

  return <APIKeysForm />;
};

export default ApiKeysPage;
