import { PublicAnalytics } from "@/components/analytics";
import { PublicSidebar } from "@/components/public-sidebar";
import UserPilotInit from "@/components/userpilot-init";

const PublicLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <>
      <PublicAnalytics />
      <UserPilotInit />
      <div className="h-full">
        <div className="hidden md:flex h-full w-20 flex-col fixed inset-y-0 z-40">
          <PublicSidebar />
        </div>
        <main className="md:pl-20 pt-20 md:pt-0 h-full">{children}</main>
      </div>
    </>
  );
};

export default PublicLayout;
