"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
import LandingTerms from "@/components/landing-terms";
import { AppdirectSvg } from "@/components/svg/appdirect-svg";
import { Button } from "@/components/ui/button";
import { useSignIn, useUser } from "@clerk/clerk-react";
import { OAuthStrategy } from "@clerk/types";
import { Loader } from "lucide-react";
import Link from "next/link";
import { useRouter } from "next/navigation";
import { useEffect, useState } from "react";

const Login = () => {
  const { isSignedIn } = useUser();
  const { isLoaded, signIn, setActive } = useSignIn();
  const [emailAddress, setEmailAddress] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);
  const [ssoLoading, setSsoLoading] = useState(false);
  const router = useRouter();

  useEffect(() => {
    if (isSignedIn) {
      router.push("/");
    }
  }, [isSignedIn]);

  const signInWith = (strategy: OAuthStrategy) => {
    if (signIn) {
      setSsoLoading(true);
      return signIn.authenticateWithRedirect({
        strategy,
        redirectUrl: "/",
        redirectUrlComplete: "/",
      });
    }
  };

  const handleSubmit = async (e: any) => {
    e.preventDefault();
    if (!isLoaded || !password || !emailAddress) {
      return;
    }
    setLoading(true);

    try {
      const result = await signIn.create({
        identifier: emailAddress,
        password,
      });

      if (result.status === "complete") {
        await setActive({ session: result.createdSessionId });
        router.push("/");
      }
    } catch (err: any) {
      setLoading(false);
      setError(err.errors[0].message || "An error occurred");
      console.error(JSON.stringify(err, null, 2));
    }
  };

  return (
    <div className="bg-coral md:bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex flex-col items-center justify-center">
        <div className="bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2 mt-16">
          <h1 className="text-3xl mb-12 font-bold">Log in</h1>
          <div className="text-red-500 text-sm pt-4">{error}</div>
          <div className="flex flex-col gap-8 mt-8 w-full md:w-80">
            <input
              type="email"
              placeholder="Email"
              className="rounded-md w-full h-12 px-4 bg-white"
              onChange={(e) => setEmailAddress(e.target.value)}
              id="email"
              name="email"
            />
            <div className="w-full">
              <input
                type="password"
                placeholder="Password"
                className="rounded-md w-full h-12 px-4 bg-white"
                onChange={(e) => setPassword(e.target.value)}
                id="password"
                name="password"
              />
              <Link
                href="/reset"
                className="underline text-xs block text-right w-full pt-1"
              >
                Forgot your password?
              </Link>
            </div>
            <Button variant="login" onClick={handleSubmit}>
              Continue
              {loading || isSignedIn ? (
                <Loader className="w-4 h-4 ml-2 spinner" />
              ) : null}
            </Button>
          </div>
          <LandingTerms />
        </div>
        <div className="w-full flex flex-col items-center gap-4 z-10 text-white">
          <div className="text-xs mt-8">
            Don&apos;t have an account?{" "}
            <Link href="/signup" className="underline">
              Sign up
            </Link>
          </div>
          <AppdirectSvg className="h-10 w-10 hidden md:block" fill="white" />
        </div>
        <BlobAnimation />
      </div>
    </div>
  );
};

export default Login;
