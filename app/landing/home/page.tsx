import LandingNav from "@/components/landing-nav";
import LandingStartChat from "@/components/landing-start-chat";
import { ArrowRight } from "lucide-react";
import Link from "next/link";

const LandingHome = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />
      <div className="max-h-[785px] h-screen pb-16 overflow-hidden flex flex-col">
        <div className="flex justify-center">
          <div className="mt-24 mr-16 w-[440px] text-center lg:text-left">
            <h2
              className="me-4 mb-8 font-extrabold leading-none tracking-tight text-6xl"
              title="AI made simple"
            >
              AppDirect AI Marketplace & Creation Studio
            </h2>
            <div className="mb-8">
              Create custom AIs for yourself, your team, or your customers in
              under 30 minutes. No coding required.
            </div>
            <Link href="/sign-up" className="ml-10 px-8 py-2 bg-sky">
              Sign up
            </Link>
            <Link href="/sign-up" className="ml-10 px-4 py-2">
              Take tour
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
          <div className="mt-20 shadow-glow">
            <video width="640" height="420" preload="none" autoPlay loop muted>
              <source src="/create-demo.mp4" type="video/mp4" />
            </video>
          </div>
        </div>
      </div>

      <LandingStartChat />

      <div className="flex flex-col items-center mb-14 mt-20">
        <div className="bg-unleash-pattern w-[1110px] px-20 py-16 flex flex-col items-center">
          <h3 className="text-3xl font-bold mb-16">
            Unleash productivity and innovation
          </h3>
          <div className="grid grid-cols-3 gap-8">
            <div className="bg-white px-8 py-8 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-11">Low/no code</h4>
              <div>
                Our low/no code solution empowers you to effortlessly create
                tailor-made AI applications for your business and clientele.
              </div>
            </div>
            <div className="bg-white px-8 py-8 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-4">
                Enterprise-grade governance
              </h4>
              <div>
                Fearlessly build your AI using even the most sensitive data. Our
                admin-first approach puts IT leaders in the driver's seat,
                ensuring meticulous oversight and control over AI
                implementations.
              </div>
            </div>
            <div className="bg-white px-8 py-8 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-11">Choose your LLM</h4>
              <div>
                App creators can choose the LLM provider most suitable to meet
                their business purpose for every app they create.
              </div>
            </div>
            <div className="bg-white px-8 py-8 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-4">
                Collaborative workspace
              </h4>
              <div>
                Our workspace fosters seamless collaboration, giving teams a
                powerful tool to gain new insights, unlock innovation, and
                increase productivity.
              </div>
            </div>
            <div className="bg-white px-8 py-8 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-11">Your data, your AI</h4>
              <div>
                Train your AI with your proprietary assets to provide results
                that are important to you, then watch it unlock unique insights
                into your business.
              </div>
            </div>
            <div className="bg-white px-8 py-8 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-4">
                Custom deployment options
              </h4>
              <div>
                For companies ready to build a fully custom, enterprise-ready AI
                solution, AppDirect partner Ivado labs provides professional
                services to help develop your vision, strategy, and
                implementation. 
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default LandingHome;
