"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
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
  const router = useRouter();

  useEffect(() => {
    if (isSignedIn) {
      router.push("/");
    }
  }, [isSignedIn]);

  const signInWith = (strategy: OAuthStrategy) => {
    if (signIn) {
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
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="bg-navylight md:bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2">
          <h1 className="text-3xl mb-12 font-bold">Log in</h1>
          <Button
            className="bg-white rounded-md px-16 py-2 text-navy"
            onClick={() => signInWith("oauth_google")}
          >
            Continue with Google
          </Button>
          <div className="mt-8 flex text-white text-sm justify-stretch w-full items-center">
            <div className="border-b border-white grow h-1"></div>
            <div className="grow-0 mx-2">or</div>
            <div className="border-b border-white grow h-1"></div>
          </div>
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
            <input
              type="password"
              placeholder="Password"
              className="rounded-md w-full h-12 px-4 bg-white"
              onChange={(e) => setPassword(e.target.value)}
              id="password"
              name="password"
            />
            <Button
              className="bg-white rounded-md px-16 py-2 text-center text-navy"
              onClick={handleSubmit}
            >
              Continue
              {loading || isSignedIn ? (
                <Loader className="w-4 h-4 ml-2 spinner" />
              ) : null}
            </Button>
          </div>
          <div className="text-xs mt-14">
            By clicking “continue”, you agree to{" "}
            <Link href="" className="underline">
              terms of use
            </Link>{" "}
            and our{" "}
            <Link href="" className="underline">
              privacy policy
            </Link>
            .
          </div>
        </div>

        <BlobAnimation />
      </div>
    </div>
  );
};

export default Login;
