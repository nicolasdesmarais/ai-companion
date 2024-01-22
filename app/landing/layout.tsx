import { PublicAnalytics } from "@/components/analytics";

const LandingLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <main className="h-full bg-[#111827] overflow-auto">
      <PublicAnalytics />
      {children}
    </main>
  );
};

export default LandingLayout;
