import { PublicAnalytics } from "@/components/analytics";

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
    <main className="h-full bg-[#111827] overflow-auto">
      <PublicAnalytics />
      {children}
    </main>
  );
};

export default LandingLayout;
