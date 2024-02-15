import { PublicAnalytics } from "@/components/analytics";
import UserPilotInit from "@/components/userpilot-init";

interface Props {
  children: React.ReactNode;
}

const PublicLayout = ({ children }: Props) => {
  return (
    <>
      <PublicAnalytics />
      <UserPilotInit />
      {children}
    </>
  );
};

export default PublicLayout;
