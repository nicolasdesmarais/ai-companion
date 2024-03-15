import LandingFAQ from "@/components/landing-faq";
import LandingFooter from "@/components/landing-footer";
import LandingNav from "@/components/landing-nav";
import LandingSupport from "@/components/landing-support";
import Link from "next/link";

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8]">
        <div className="lg:w-[1140px] mx-4">
          <h2 className="text-5xl font-bold">Pricing</h2>
        </div>
      </div>

      <div className="flex items-center justify-center my-16">
        <div className="lg:w-[1100px] mx-4 flex flex-col items-center">
          <h2 className="text-3xl font-bold text-center mb-8">
            Choose the plan that&apos;s right for you
          </h2>
          <Link href="/sign-up" className="px-8 py-2 bg-sky">
            Sign up
          </Link>
          <div className="grid mt-16 gap-8 grid-cols-1 md:grid-cols-2 lg:grid-cols-3"></div>
        </div>
      </div>

      <LandingFAQ />

      <LandingSupport />

      <LandingFooter />
    </div>
  );
};

export default LandingFeatures;
