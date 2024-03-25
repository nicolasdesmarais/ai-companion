import LandingNav from "@/components/landing-nav";
import Link from "next/link";

const Login = () => {
  return (
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="bg-gradient4 absolute z-10 rounded-lg flex flex-col items-center p-16">
          <h1 className="text-3xl mb-12 font-bold">Log in</h1>
          <div className="bg-white rounded-md px-16 py-2">
            Continue with Google
          </div>
          <div className="mt-8 flex text-white text-sm justify-stretch w-full items-center">
            <div className="border-b border-white grow h-1"></div>
            <div className="grow-0 mx-2">or</div>
            <div className="border-b border-white grow h-1"></div>
          </div>
          <div className="flex flex-col gap-8 mt-8">
            <input
              type="text"
              placeholder="Email"
              className="rounded-md w-80 h-12 px-4 bg-white"
            />
            <input
              type="password"
              placeholder="Password"
              className="rounded-md w-80 h-12 px-4 bg-white"
            />
            <div className="bg-white rounded-md px-16 py-2 text-center">
              Continue
            </div>
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

        <svg xmlns="http://www.w3.org/2000/svg" className="hidden">
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
