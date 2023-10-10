import { auth, redirectToSignIn } from "@clerk/nextjs";
import { CreateGroupForm } from "./components/create-group-form";

const GroupPage = async () => {
  const authorization = await auth();

  if (!authorization?.userId) {
    return redirectToSignIn();
  }

  return <CreateGroupForm />;
};

export default GroupPage;
