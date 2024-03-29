"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
import LandingTerms from "@/components/landing-terms";
import { AppdirectSvg } from "@/components/svg/appdirect-svg";
import { Button } from "@/components/ui/button";
import { useSignUp, useUser } from "@clerk/clerk-react";
import { OAuthStrategy } from "@clerk/types";
import { Eye, EyeOff, Loader } from "lucide-react";
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
  const [type, setType] = useState("password");
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

  const handleToggle = () => {
    if (type === "password") {
      setType("text");
    } else {
      setType("password");
    }
  };

  return (
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex flex-col items-center justify-center">
        <div className="bg-navylight md:bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2 mt-16">
          <h1 className="text-3xl mb-12 font-bold">Create your Account</h1>
          {pendingVerification ? (
            <>
              <div>Check your email for a verification code</div>
              <div className="flex flex-col gap-8 mt-8">
                <input
                  value={code}
                  className="rounded-md w-80 h-12 px-4 bg-white"
                  placeholder="Verification Code"
                  onChange={(e) => setCode(e.target.value)}
                />
                <Button
                  className="bg-white rounded-md px-16 py-2 text-center text-navy"
                  onClick={onPressVerify}
                >
                  Verify Email
                </Button>
              </div>
            </>
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
              <div className="flex flex-col gap-8 mt-8 w-full md:w-80">
                <input
                  type="email"
                  placeholder="Email"
                  className="rounded-md w-full h-12 px-4 bg-white"
                  onChange={(e) => setEmailAddress(e.target.value)}
                  id="email"
                  name="email"
                />
                <div className="relative">
                  <input
                    type={type}
                    placeholder="Password"
                    className="rounded-md w-full h-12 px-4 bg-white"
                    onChange={(e) => setPassword(e.target.value)}
                    id="password"
                    name="password"
                  />
                  <span
                    className="absolute top-3 right-10 cursor-pointer"
                    onClick={handleToggle}
                  >
                    {type === "password" ? (
                      <Eye className="absolute" />
                    ) : (
                      <EyeOff className="absolute" />
                    )}
                  </span>
                </div>

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
              <LandingTerms />
            </>
          )}
        </div>
        <div className="w-full flex flex-col items-center gap-4 z-10 md:text-white">
          <div className="text-xs mt-8">
            Already have an account?{" "}
            <Link href="/login" className="underline">
              Log in
            </Link>
          </div>
          <AppdirectSvg className="h-10 w-10 hidden md:block" fill="white" />
          <AppdirectSvg className="h-10 w-10 md:hidden" />
        </div>
        <BlobAnimation />
      </div>
    </div>
  );
};

export default Login;
