"use client";
import BlobAnimation from "@/components/blob-animation";
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
              afterSelectOrganizationUrl="/index/public"
              afterCreateOrganizationUrl="/index/public"
              hidePersonal={true}
            />
          </div>
        </div>

        <BlobAnimation />
      </div>
    </div>
  );
};

export default OrgSelect;
