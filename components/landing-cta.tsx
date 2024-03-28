import BlobAnimation from "@/components/blob-animation";
import { ArrowRight } from "lucide-react";
import Link from "next/link";

const LandingCTA = () => {
  return (
    <div className="flex flex-col items-center mt-20 relative overflow-hidden blob-container">
      <div className="lg:w-[1140px] py-28 px-10 lg:px-32 z-10">
        <div className="flex flex-col lg:flex-row bg-white py-12 px-10 lg:px-20 justify-evenly items-center">
          <h3 className="text-3xl font-bold lg:w-[440px]">
            Start creating AI apps today.
          </h3>
          <Link
            href="/sign-up"
            className="mt-8 lg:mt-0 lg:ml-10 px-8 py-2 bg-navy text-white flex items-center"
          >
            Get Started
            <ArrowRight className="inline-block w-4 h-4 ml-2" />
          </Link>
        </div>
      </div>
      <BlobAnimation />
    </div>
  );
};

export default LandingCTA;
