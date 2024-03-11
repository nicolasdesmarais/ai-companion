import LandingCTA from "@/components/landing-cta";
import LandingFooter from "@/components/landing-footer";
import LandingNav from "@/components/landing-nav";
import { ArrowRight } from "lucide-react";
import Image from "next/image";
import Link from "next/link";

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8]">
        <div className="md:w-[1140px] ">
          <h2 className="text-5xl font-extrabold">How it works</h2>
          <div className="md:w-[700px] mt-10 text-lg">
            Transform your AI app ideas into reality without needing any coding
            skills. Unlock innovation and productivity for you, your team, and
            your customers.
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center mt-20">
        <div className="flex">
          <div className="md:w-[460px] gap-8 flex flex-col ">
            <div>Create</div>
            <div className="text-3xl font-extrabold md:w-[300px]">
              Custom AI apps, created in minutes
            </div>
            <ul className="list-disc gap-4 flex flex-col ml-4">
              <li>
                <b>Advanced data ingestion options:</b> AppDirect AI provides
                more built-in choices than any other solution to define your
                app&nbsp;s data sources (files, drive folders, and websites) and
                schedule automated data updates.
              </li>
              <li>
                <b>App personality optimization:</b> Customize your AI app to
                speak with users in an appropriate language style for the task.
              </li>
              <li>
                <b>Secure environment:</b> Build your AI app on your proprietary
                data sets in a secure environment without worry.
              </li>
            </ul>
            <Link href="/sign-up" className="py-2">
              Start Building
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
          <div className="bg-cta-pattern p-20 md:w-[600px] ml-20">
            <Image
              src="/datasources_screenshot.jpg"
              alt="AI Data Source Screenshot"
              width="512"
              height="360"
            />
          </div>
        </div>
      </div>

      <LandingCTA />

      <LandingFooter />
    </div>
  );
};

export default LandingFeatures;
