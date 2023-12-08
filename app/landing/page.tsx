import { AITeaser } from "@/components/ai-teaser";
import Image from "next/image";
const LandingPage = () => {
  return (
    <div className="blue-bg flex flex-col h-full">
      <div className="flex justify-center h-full my-16">
        <div className="auth-side-panel hidden md:block">
          <h2
            className="me-4 mb-4 font-extrabold leading-none tracking-tight text-gray-900 text-4xl dark:text-white"
            title=" AI made simple"
          >
            Upgrade the way you work
          </h2>
          <div>
            Build your own custom AIs or browse the AppDirect AI Marketplace for
            AIs created by our community. No code required.
          </div>
        </div>
      </div>

      <AITeaser />

      <div className="flex justify-center items-center flex-col">
        <Image
          src="/AppDirect-Mark_White.png"
          alt="AppDirect Logo"
          width="64"
          height="64"
          className="mt-5"
        />
        <h1 className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
          AppDirect AI
        </h1>
        <h2 className="mt-4">Coming Soon...</h2>
      </div>
    </div>
  );
};

export default LandingPage;
