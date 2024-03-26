"use client";
import LandingNav from "@/components/landing-nav";
import { OrganizationList } from "@clerk/nextjs";

const OrgSelect = () => {
  return (
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="absolute z-10 flex flex-col items-center ">
          <div className="flex flex-col gap-8 mt-8">
            <OrganizationList
              afterSelectOrganizationUrl="/"
              afterCreateOrganizationUrl="/"
              hidePersonal={true}
            />
          </div>
        </div>

        <svg xmlns="http://www.w3.org/2000/svg" className="svg-filter">
          <defs>
            <filter id="goo">
              <feGaussianBlur
                in="SourceGraphic"
                stdDeviation="10"
                result="blur"
              />
              <feColorMatrix
                in="blur"
                mode="matrix"
                values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 18 -8"
                result="goo"
              />
              <feBlend in="SourceGraphic" in2="goo" />
            </filter>
          </defs>
        </svg>
        <div className="h-full w-full blob-background overflow-hidden">
          <div className="blob-one"></div>
          <div className="blob-two"></div>
          <div className="blob-three"></div>
          <div className="blob4"></div>
          <div className="blob5"></div>
        </div>
      </div>
    </div>
  );
};

export default OrgSelect;
