import Link from "next/link";
import { AppdirectSvg } from "./svg/appdirect-svg";

const LandingNav = () => {
  return (
    <div className="flex text-navy justify-center w-full z-10 bg-white">
      <nav className="flex m-4 w-[1144px] justify-between">
        <div className="flex">
          <Link
            href="/landing/home"
            className="flex items-center md:mr-10 lg:mr-20"
          >
            <AppdirectSvg className="h-5 w-5" />
            AppDirect <span className="font-extrabold ml-2">AI</span>
          </Link>
          <ul className="hidden md:flex justify-between md:gap-8 lg:gap-14 py-2">
            <li>
              <Link href="/landing/features">How it works</Link>
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
        <div className="flex">
          <Link href="/landing/login" className="py-2">
            Login
          </Link>
          <Link href="/sign-up" className="ml-4 lg:ml-10 px-4 py-2 bg-sky">
            Sign up
          </Link>
        </div>
      </nav>
    </div>
  );
};

export default LandingNav;
