import { KnowledgeManager } from "@/components/knowledge-manager";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

const superusers = ["user_2WXwDNSh5x5zxnabNzpSN2Z1JWs"];

const SettingsPage = async () => {
  const { userId } = await auth();

  if (!userId) {
    return redirectToSignIn();
  }

  if (!superusers.includes(userId)) {
    console.log("Superuser attempt", userId);
    return redirect("/");
  }

  return (
    <div className="h-full p-4 space-y-2">
      <h3 className="text-lg font-medium">Knowledge Uploads</h3>
      <div className="text-muted-foreground text-sm">
        <KnowledgeManager />
      </div>
    </div>
  );
};

export default SettingsPage;
