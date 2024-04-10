"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
import { Button } from "@/components/ui/button";
import { useClerk } from "@clerk/nextjs";
import { Loader } from "lucide-react";
import Image from "next/image";
import { useRouter } from "next/navigation";
import { useEffect, useState } from "react";

const OrgSelect = () => {
  const [loading, setLoading] = useState(true);
  const [invitations, setInvitations] = useState<any[]>([]);
  const [error, setError] = useState("");
  const [joining, setJoining] = useState(false);
  const [company, setCompany] = useState("");
  const [creating, setCreating] = useState(false);
  const clerk = useClerk();
  const router = useRouter();

  useEffect(() => {
    const fetchInvitations = async () => {
      if (clerk.user) {
        if (clerk.user.organizationMemberships.length > 0) {
          const organization =
            clerk.user.organizationMemberships[0].organization.id;
          await clerk.setActive({
            session: clerk.session?.id,
            organization,
          });
          router.push("/");
          return;
        } else {
          const result = await clerk.user.getOrganizationInvitations();
          setInvitations(result.data);
          setLoading(false);
        }
      }
    };
    fetchInvitations();
  }, [clerk.user]);

  const join = async (invitation: any) => {
    setJoining(true);
    try {
      const result = await invitation.accept();
      if (result.status === "accepted") {
        const organization = result.publicOrganizationData.id;
        await clerk.setActive({
          session: clerk.session?.id,
          organization,
        });
        router.push("/index/public");
      }
    } catch (e) {
      console.error(e);
      setJoining(false);
      setError(e.errors[0].message || "An error occurred");
    }
  };

  const create = async (e: any) => {
    e.preventDefault();
    setCreating(true);
    try {
      const org = await clerk.createOrganization({ name: company });
      await clerk.setActive({
        session: clerk.session?.id,
        organization: org.id,
      });
      router.push("/index/public");
    } catch (err: any) {
      setCreating(false);
      setError(err.errors[0].message || "An error occurred");
      console.error(JSON.stringify(err, null, 2));
    }
  };

  return (
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="absolute z-10 flex flex-col items-center ">
          <div className="h-full w-full flex flex-col items-center justify-center">
            <div className="bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2 mt-16">
              {loading ? (
                <Loader className="w-16 h-16 spinner" />
              ) : (
                <>
                  <h1 className="text-3xl mb-12 font-bold">Your Company</h1>
                  <div className="text-red-500 text-sm pt-4">{error}</div>
                  <div>
                    {invitations.map((invitation: any) => (
                      <div key={invitation.id}>
                        <div className="flex pl-4 pt-4 mb-1">
                          <Image
                            alt={invitation.publicOrganizationData.name}
                            src={invitation.publicOrganizationData.imageUrl}
                            width="44"
                            height="44"
                            className="rounded-lg"
                          />
                          <div className="truncate w-56 font-bold text-lg flex items-center px-6">
                            {invitation.publicOrganizationData.name}
                          </div>
                          <Button
                            variant="login"
                            onClick={() => join(invitation)}
                          >
                            Join
                            {joining ? (
                              <Loader className="w-4 h-4 ml-2 spinner" />
                            ) : null}
                          </Button>
                        </div>
                      </div>
                    ))}
                  </div>
                  {invitations.length > 0 ? (
                    <div className="mt-8 flex text-white text-sm justify-stretch w-full items-center mb-8">
                      <div className="border-b border-white grow h-1"></div>
                      <div className="grow-0 mx-2">or</div>
                      <div className="border-b border-white grow h-1"></div>
                    </div>
                  ) : null}
                  <div className="flex flex-col gap-8 w-full md:w-80">
                    <input
                      type="text"
                      placeholder="Company Name"
                      className="rounded-md w-full h-12 px-4 bg-white"
                      onChange={(e) => setCompany(e.target.value)}
                      id="company"
                      name="company"
                    />

                    <Button variant="login" onClick={create}>
                      Create New Company
                      {creating ? (
                        <Loader className="w-4 h-4 ml-2 spinner" />
                      ) : null}
                    </Button>
                  </div>
                </>
              )}
            </div>
          </div>
        </div>

        <BlobAnimation />
      </div>
    </div>
  );
};

export default OrgSelect;
