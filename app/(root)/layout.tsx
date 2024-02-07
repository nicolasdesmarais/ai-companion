import DatadogInit from "@/components/datadog-init";
import { Navbar } from "@/components/navbar";
import PendoInit from "@/components/pendo-init";
import { Sidebar } from "@/components/sidebar";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";

const RootLayout = async ({ children }: { children: React.ReactNode }) => {
  const authorizationContext = getUserAuthorizationContext();
  if (!authorizationContext) {
    return;
  }

  const { userId, permissions, orgId } = authorizationContext;
  if (!userId) {
    return;
  }

  return (
    <>
      <DatadogInit userId={userId} />
      <PendoInit userId={userId} orgId={orgId} />
      <div className="h-full">
        <Navbar isPro={false} userPermissions={permissions} orgId={orgId} />
        <div className="hidden md:flex h-full w-20 flex-col fixed inset-y-0 z-40">
          <Sidebar isPro={false} userPermissions={permissions} orgId={orgId} />
        </div>
        <main className="md:pl-20 pt-20 md:pt-0 h-full">{children}</main>
      </div>
    </>
  );
};

export default RootLayout;
