import { SuperDataSources } from "@/components/super-data-sources";
import { isSuperuser } from "@/src/lib/utils";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { redirect } from "next/navigation";

const SettingsPage = async () => {
  const { userId } = await auth();

  if (!userId) {
    return redirectToSignIn();
  }

  if (!isSuperuser(userId)) {
    console.log("Superuser attempt", userId);
    return redirect("/");
  }

  return (
    <div className="h-full p-4 space-y-2">
      <h3 className="text-lg font-medium">All Data Sources</h3>
      <div className="text-muted-foreground text-sm">
        <SuperDataSources />
      </div>
    </div>
  );
};

export default SettingsPage;
