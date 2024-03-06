import LandingNav from "@/components/landing-nav";
import LandingStartChat from "@/components/landing-start-chat";
import { ArrowRight } from "lucide-react";
import Image from "next/image";
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

      <div className="flex flex-col lg:flex-row justify-center p-8 lg:p-14 items-center lg:items-start">
        <div className="md:m-8 lg:w-96 space-y-4">
          <h3 className="mt-5 text-3xl font-bold">Interact with AIs</h3>
          <div>
            Chat with AIs to ask broad questions that help you summarize complex
            data sets or dig into the details quickly with specific queries.
          </div>
          <div>
            AIs learn what is most helpful for you with every interaction.
          </div>
        </div>
        <Image
          src="/interact_screenshot.jpg"
          alt="Chat Page"
          width="512"
          height="360"
          className="mt-5 shadow-glow"
        />
      </div>

      <div className="flex flex-col lg:flex-row justify-center p-8 lg:p-14 items-center lg:items-start">
        <Image
          src="/share_screenshot.jpg"
          alt="Chat Page"
          width="512"
          height="377"
          className="mt-5 shadow-glow"
        />
        <div className="md:ml-8 mt-4 lg:w-[400px] space-y-4 text-left lg:text-right">
          <h3 className="mt-5 text-3xl font-bold">Share with confidence</h3>
          <div>
            Sharing your AI bot securely is paramount. AppDirect delivers robust
            enterprise-grade access control.
          </div>
          <div>
            You can specify who can interact with your bot and to what extent,
            safeguarding sensitive information and ensuring proper usage.
          </div>
        </div>
      </div>
    </div>
  );
};

export default LandingHome;
