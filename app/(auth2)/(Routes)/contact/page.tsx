"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
import { Button } from "@/components/ui/button";
import { Loader } from "lucide-react";
import { useRouter } from "next/navigation";
import { useState } from "react";

const Login = () => {
  const [emailAddress, setEmailAddress] = useState("");
  const [name, setName] = useState("");
  const [company, setCompany] = useState("");
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);
  const router = useRouter();

  const handleSubmit = async (e: any) => {
    e.preventDefault();
    if (!name || !emailAddress || !company) {
      return;
    }
    setLoading(true);

    try {
      console.log("haha");
    } catch (err: any) {
      setLoading(false);
      setError(err.errors[0].message || "An error occurred");
      console.error(JSON.stringify(err, null, 2));
    }
  };

  return (
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="bg-navylight md:bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2">
          <h1 className="text-3xl mb-12 font-bold">AI for the Enterprise</h1>
          <div>Connect with our Enterprise team today.</div>
          <div className="text-red-500 text-sm pt-4">{error}</div>
          <div className="flex flex-col gap-8 mt-8 w-full">
            <input
              type="text"
              placeholder="Name"
              className="rounded-md md:w-80 h-12 px-4 bg-white"
              onChange={(e) => setName(e.target.value)}
              id="name"
              name="name"
            />
            <input
              type="text"
              placeholder="Company"
              className="rounded-md md:w-80 h-12 px-4 bg-white"
              onChange={(e) => setCompany(e.target.value)}
              id="company"
              name="company"
            />
            <input
              type="email"
              placeholder="Work email"
              className="rounded-md md:w-80 h-12 px-4 bg-white"
              onChange={(e) => setEmailAddress(e.target.value)}
              id="email"
              name="email"
            />
            <Button
              className="bg-white rounded-md px-16 py-2 text-center text-navy"
              onClick={handleSubmit}
            >
              Contact Us
              {loading ? <Loader className="w-4 h-4 ml-2 spinner" /> : null}
            </Button>
          </div>
        </div>

        <BlobAnimation />
      </div>
    </div>
  );
};

export default Login;
