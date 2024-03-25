import LandingFAQ from "@/components/landing-faq";
import LandingFooter from "@/components/landing-footer";
import LandingPricing from "@/components/landing-pricing";
import LandingSupport from "@/components/landing-support";

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8] ">
        <div className="lg:w-[1140px] mx-4">
          <h2 className="text-5xl font-bold">Pricing</h2>
          <div className="lg:w-[700px] mt-10 text-lg">
            Our pricing plans are based on data usage, starting with a free plan
            to get you started right away.
          </div>
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
