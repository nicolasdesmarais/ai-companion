import Link from "next/link";
import { AppdirectSvg } from "./svg/appdirect-svg";

const LandingFooter = () => {
  const currentYear = new Date().getFullYear();
  return (
    <div className="flex text-white bg-navy justify-center w-full mt-20 py-8">
      <nav className="flex my-4 w-[1144px] justify-between">
        <div className="w-full">
          <div className="flex gap-20">
            <div className="flex mr-40">
              <AppdirectSvg className="h-5 w-5" fill="white" />
              AppDirect <span className="font-extrabold ml-2">AI</span>
            </div>
            <div>
              <div className="font-bold">PRODUCT</div>
              <ul className="flex flex-col justify-between gap-4 py-2 font-light">
                <li>
                  <Link href="/landing/features">Features</Link>
                </li>
                <li>
                  <Link href="/landing/pricing">Pricing</Link>
                </li>
                <li>
                  <Link href="/landing/enterprise">Enterprise</Link>
                </li>
                <li>
                  <Link href="/landing/resources">Resources</Link>
                </li>
              </ul>
            </div>
            <div>
              <div className="font-bold">RESOURCES</div>
              <ul className="flex flex-col justify-between gap-4 py-2 font-light">
                <li>
                  <Link href="/landing/home">Tutorials</Link>
                </li>
                <li>
                  <Link href="/landing/home">User Guides</Link>
                </li>
                <li>
                  <Link href="/landing/home">API Guides</Link>
                </li>
                <li>
                  <Link href="/landing/home">FAQ</Link>
                </li>
              </ul>
            </div>
            <div>
              <div className="font-bold">LEGAL</div>
              <ul className="flex flex-col justify-between gap-4 py-2 font-light">
                <li>
                  <Link href="/landing/home">Privacy Notice</Link>
                </li>
                <li>
                  <Link href="/landing/home">Terms of Use</Link>
                </li>
              </ul>
            </div>
            <div>
              <div className="font-bold">SOCIAL</div>
              <ul className="flex flex-col justify-between gap-4 py-2 font-light">
                <li>
                  <Link href="/landing/home">LinkedIn</Link>
                </li>
                <li>
                  <Link href="/landing/home">YouTube</Link>
                </li>
                <li>
                  <Link href="/landing/home">Instagram</Link>
                </li>
              </ul>
            </div>
          </div>
          <div className="flex justify-between w-full mt-8">
            <div>{`AppDirect ©${currentYear}`}</div>
            <Link
              href="/sign-up"
              className="ml-10 px-6 py-2 bg-white text-navy"
            >
              Sign up
            </Link>
          </div>
        </div>
      </nav>
    </div>
  );
};

export default LandingFooter;
