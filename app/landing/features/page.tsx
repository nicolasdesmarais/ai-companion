import LandingCTA from "@/components/landing-cta";
import LandingFooter from "@/components/landing-footer";
import LandingNav from "@/components/landing-nav";

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8]">
        <div className="md:w-[1100px] ">
          <h2 className="text-5xl font-extrabold">How it works</h2>
          <div className="md:w-[700px] mt-10 text-lg">
            Transform your AI app ideas into reality without needing any coding
            skills. Unlock innovation and productivity for you, your team, and
            your customers.
          </div>
        </div>
      </div>

      <LandingCTA />

      <LandingFooter />
    </div>
  );
};

export default LandingFeatures;
