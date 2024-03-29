import { PublicAnalytics } from "@/components/analytics";
import HubSpotInit from "@/components/hubspot-init";
import LandingNav from "@/components/landing-nav";

export const metadata = {
  title: `AppDirect AI`,
  openGraph: {
    images: [
      {
        url: "/browse_screenshot.jpg",
        width: "512",
        height: "377",
      },
    ],
    description:
      "Transform your AI app ideas into reality without needing any coding skills. Unlock innovation and productivity for you, your team, and your customers.",
    siteName: "AppDirect AI Marketplace & Creation Studio",
    locale: "en_US",
    type: "website",
  },
};

const LandingLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <>
      <LandingNav />
      <HubSpotInit />
      <PublicAnalytics />
      <main className="h-full bg-white overflow-auto pt-[72px]">
        {children}
      </main>
    </>
  );
};

export default LandingLayout;
