import LandingFooter from "@/components/landing-footer";
import LandingLLM from "@/components/landing-llm";
import LandingNav from "@/components/landing-nav";

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center ">
        <div className="lg:w-[1140px] mx-4">
          <h2 className="text-5xl font-bold">Resources</h2>
          <div className="lg:w-[700px] mt-10 text-lg">
            Guides, tutorials, and reference materials to help you on your
            creation journey.
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center mt-8 mb-12 lg:mx-0 bg-[#F8F8F8] px-4 lg:px-0 lg:py-14">
        <div className="flex flex-col-reverse lg:flex-row">
          <div className="flex justify-center items-center lg:w-[560px] lg:w-[524px] mt-8 lg:mt-0">
            <video width="640" height="420" preload="none" autoPlay loop muted>
              <source src="/create-demo.mp4" type="video/mp4" />
            </video>
          </div>
          <div className="lg:w-[460px] gap-8 flex flex-col mt-10 lg:mt-0 lg:ml-20 justify-center">
            <div className="text-3xl font-bold">Start here with a tour</div>
            <div>
              Watch this video to learn how to use AppDirect AI effectively and
              create fit-for-purpose AIs for specific roles. 
            </div>
          </div>
        </div>
      </div>

      <LandingLLM className="bg-[#F8F8F8] py-14" />

      <LandingFooter />
    </div>
  );
};

export default LandingFeatures;
