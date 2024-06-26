"use client";
import BlobAnimation from "@/components/blob-animation";
import HubSpotInit from "@/components/hubspot-init";
import LandingNav from "@/components/landing-nav";
import LandingTerms from "@/components/landing-terms";
import { AppdirectSvg } from "@/components/svg/appdirect-svg";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import { useSignUp, useUser } from "@clerk/clerk-react";
import { OAuthStrategy } from "@clerk/types";
import { Eye, EyeOff, Loader } from "lucide-react";
import Link from "next/link";
import { useRouter } from "next/navigation";
import { useEffect, useState } from "react";

const SignUp = () => {
  const { isLoaded, signUp, setActive } = useSignUp();
  const { isSignedIn } = useUser();
  const [emailAddress, setEmailAddress] = useState("");
  const [password, setPassword] = useState("");
  const [pendingVerification, setPendingVerification] = useState(false);
  const [code, setCode] = useState("");
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);
  const [verifying, setVerifying] = useState(false);
  const [type, setType] = useState("password");
  const [googleLoading, setGoogleLoading] = useState(false);
  const router = useRouter();
  const { toast } = useToast();

  useEffect(() => {
    if (isSignedIn) {
      router.push("/");
    }
  }, [isSignedIn]);

  const signInWith = async (strategy: OAuthStrategy) => {
    if (signUp) {
      setGoogleLoading(true);
      try {
        await signUp.authenticateWithRedirect({
          strategy,
          redirectUrl: "/org-selection",
          redirectUrlComplete: "/org-selection",
        });
      } catch (err: any) {
        setGoogleLoading(false);
        setError(err.errors[0].message || "An error occurred");
        console.error(JSON.stringify(err, null, 2));
      }
    }
  };

  const handleSubmit = async (e: any) => {
    setError("");
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

  const handleResendCode = async (e: any) => {
    await handleSubmit(e);
    toast({
      description: "Verification Code Sent.",
      duration: 2000,
    });
  };

  const onPressVerify = async (e: any) => {
    setError("");
    e.preventDefault();
    if (!isLoaded) {
      return;
    }
    setVerifying(true);

    try {
      const completeSignUp = await signUp.attemptEmailAddressVerification({
        code,
      });
      if (completeSignUp.status === "complete") {
        await Promise.all([
          setActive({ session: completeSignUp.createdSessionId }),
        ]);
        router.push("/org-selection");
      } else {
        setVerifying(false);
        setError("A verification error occurred");
        console.error(JSON.stringify(completeSignUp, null, 2));
      }
    } catch (err: any) {
      setVerifying(false);
      setError(err.errors[0].longMessage || "An error occurred");
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

  const handleEnter = (event: any, submit: (e: any) => void) => {
    switch (event.key) {
      case "Enter":
        submit(event);
        break;
      default:
        break;
    }
  };

  return (
    <div className="bg-coral md:bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />
      <HubSpotInit />
      <div className="h-full w-full flex flex-col items-center justify-center">
        <div className="bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2 mt-16">
          <h1 className="text-3xl mb-12 font-bold">Create your Account</h1>
          {pendingVerification ? (
            <>
              <div>Check your email for a verification code</div>
              <div className="text-red-500 text-sm pt-4">{error}</div>
              <div className="flex flex-col gap-8 mt-8">
                <div className="w-full">
                  <input
                    value={code}
                    className="rounded-md w-80 h-12 px-4 bg-white"
                    placeholder="Verification Code"
                    onChange={(e) => setCode(e.target.value)}
                    onKeyDown={(e) => handleEnter(e, onPressVerify)}
                  />
                  <span
                    className="underline text-xs block text-right w-full pt-1 cursor-pointer"
                    onClick={handleResendCode}
                  >
                    Resend Code
                  </span>
                </div>
                <Button variant="login" onClick={onPressVerify}>
                  Verify Email
                  {verifying ? (
                    <Loader className="w-4 h-4 ml-2 spinner" />
                  ) : null}
                </Button>
              </div>
            </>
          ) : (
            <>
              <Button
                variant="login"
                size="wide"
                onClick={() => signInWith("oauth_google")}
              >
                Continue with Google
                {googleLoading ? (
                  <Loader className="w-4 h-4 ml-2 spinner" />
                ) : null}
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
                    onKeyDown={(e) => handleEnter(e, handleSubmit)}
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

                <Button variant="login" onClick={handleSubmit}>
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
        <div className="w-full flex flex-col items-center gap-4 z-10 text-white">
          <div className="text-xs mt-8">
            Already have an account?{" "}
            <Link href="/login" className="underline">
              Log in
            </Link>
          </div>
          <AppdirectSvg className="h-10 w-10 hidden md:block" fill="white" />
        </div>
        <BlobAnimation />
      </div>
    </div>
  );
};

export default SignUp;
