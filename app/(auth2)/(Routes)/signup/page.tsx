"use client";
import LandingNav from "@/components/landing-nav";
import { Button } from "@/components/ui/button";
import { useSignUp, useUser } from "@clerk/clerk-react";
import { OAuthStrategy } from "@clerk/types";
import { Loader } from "lucide-react";
import Link from "next/link";
import { useRouter } from "next/navigation";
import { useEffect, useState } from "react";

const Login = () => {
  const { isLoaded, signUp, setActive } = useSignUp();
  const { isSignedIn } = useUser();
  const [emailAddress, setEmailAddress] = useState("");
  const [password, setPassword] = useState("");
  const [pendingVerification, setPendingVerification] = useState(false);
  const [code, setCode] = useState("");
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);
  const router = useRouter();

  useEffect(() => {
    if (isSignedIn) {
      router.push("/");
    }
  }, [isSignedIn]);

  const signInWith = (strategy: OAuthStrategy) => {
    if (signUp) {
      return signUp.authenticateWithRedirect({
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
      await signUp.create({
        emailAddress,
        password,
      });

      await signUp.prepareEmailAddressVerification({ strategy: "email_code" });

      setPendingVerification(true);
    } catch (err: any) {
      setLoading(false);
      setError(err.errors[0].message || "An error occurred");
      console.error(JSON.stringify(err, null, 2));
    }
  };

  const onPressVerify = async (e: any) => {
    e.preventDefault();
    if (!isLoaded) {
      return;
    }

    try {
      const completeSignUp = await signUp.attemptEmailAddressVerification({
        code,
      });
      if (completeSignUp.status === "complete") {
        await setActive({ session: completeSignUp.createdSessionId });
        router.push("/");
      } else {
        console.error(JSON.stringify(completeSignUp, null, 2));
      }
    } catch (err: any) {
      console.error(JSON.stringify(err, null, 2));
    }
  };

  return (
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="bg-gradient4 absolute z-10 rounded-lg flex flex-col items-center p-16">
          <h1 className="text-3xl mb-12 font-bold">Create your Account</h1>
          {pendingVerification ? (
            <div>
              <form>
                <input
                  value={code}
                  placeholder="Code..."
                  onChange={(e) => setCode(e.target.value)}
                />
                <button onClick={onPressVerify}>Verify Email</button>
              </form>
            </div>
          ) : (
            <>
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
              <div className="flex flex-col gap-8 mt-8">
                <input
                  type="email"
                  placeholder="Email"
                  className="rounded-md w-80 h-12 px-4 bg-white"
                  onChange={(e) => setEmailAddress(e.target.value)}
                  id="email"
                  name="email"
                />
                <input
                  type="password"
                  placeholder="Password"
                  className="rounded-md w-80 h-12 px-4 bg-white"
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
            </>
          )}
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

export default Login;
