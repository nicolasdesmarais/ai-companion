"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
import { useUser } from "@clerk/nextjs";

const OrgSelect = () => {
  const { user } = useUser();
  return (
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="absolute z-10 flex flex-col items-center ">
          <div className="h-full w-full flex flex-col items-center justify-center">
            <div className="bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2 mt-16">
              <h1 className="text-3xl mb-12 font-bold">Select your Company</h1>
              <div>haha</div>
            </div>
          </div>
        </div>

        <BlobAnimation />
      </div>
    </div>
  );
};

export default OrgSelect;
