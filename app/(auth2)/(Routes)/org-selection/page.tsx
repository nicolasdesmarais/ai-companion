"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
import { useClerk } from "@clerk/nextjs";
import Image from "next/image";
import { useEffect, useState } from "react";

const OrgSelect = () => {
  const clerk = useClerk();
  const [invitations, setInvitations] = useState<any[]>([]);

  useEffect(() => {
    const fetchInvitations = async () => {
      if (clerk.user) {
        const result = await clerk.user.getOrganizationInvitations();
        setInvitations(result.data);
        console.log(result.data);
      }
    };
    fetchInvitations();
  }, [clerk.user]);
  return (
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="absolute z-10 flex flex-col items-center ">
          <div className="h-full w-full flex flex-col items-center justify-center">
            <div className="bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2 mt-16">
              <h1 className="text-3xl mb-12 font-bold">Select your Company</h1>
              <div>
                {invitations.map((invitation: any) => (
                  <div key={invitation.id}>
                    <div className="flex flex-row pl-4 pt-4 mb-1">
                      <Image
                        alt={invitation.publicOrganizationData.name}
                        src={invitation.publicOrganizationData.imageUrl}
                        width="44"
                        height="44"
                        className="rounded-lg"
                      />
                      <div className="pl-4">
                        <div className="truncate max-w-64">
                          {invitation.publicOrganizationData.name}
                        </div>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          </div>
        </div>

        <BlobAnimation />
      </div>
    </div>
  );
};

export default OrgSelect;
