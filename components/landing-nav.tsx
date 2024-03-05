import Link from "next/link";
import { AppdirectSvg } from "./svg/appdirect-svg";

const LandingNav = () => {
  return (
    <div className="flex text-navy justify-center w-full">
      <nav className="flex my-4">
        <ul className="flex justify-between gap-8 mr-20">
          <li className="flex">
            <AppdirectSvg className="h-5 w-5" />
            AppDirect <span className="font-extrabold">AI</span>
          </li>
          <li>
            <Link href="/landing/how">How it works</Link>
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
        <Link href="/sign-in" className="ml-20">
          Login
        </Link>
        <Link href="/sign-up" className="ml-4">
          Sign up
        </Link>
      </nav>
    </div>
  );
};

export default LandingNav;
