import LandingFAQ from "@/components/landing-faq";
import LandingFooter from "@/components/landing-footer";
import LandingNav from "@/components/landing-nav";
import LandingPricing from "@/components/landing-pricing";
import LandingSupport from "@/components/landing-support";

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8]">
        <div className="lg:w-[840px] mx-4">
          <h2 className="text-5xl font-bold">Pricing</h2>
        </div>
      </div>

      <LandingPricing />

      <LandingFAQ />

      <LandingSupport />

      <LandingFooter />
    </div>
  );
};

export default LandingFeatures;
