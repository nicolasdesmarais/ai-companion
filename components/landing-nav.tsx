import { cn } from "@/src/lib/utils";
import Link from "next/link";
import { MobileNav } from "./landing-mobile-nav";
import { AppdirectSvg } from "./svg/appdirect-svg";

interface Props {
  transparent?: boolean;
}

const LandingNav = ({ transparent }: Props) => {
  return (
    <div className="fixed w-full md:pr-4 z-20">
      <div
        className={cn(
          "flex text-navy justify-center w-full bg-white",
          transparent ? "md:bg-transparent" : ""
        )}
      >
        <nav className="flex m-4 w-[1144px] justify-between">
          <div className="flex">
            <MobileNav />
            <Link
              href="/landing/"
              className="flex items-center md:mr-10 lg:mr-20"
            >
              <AppdirectSvg className="h-5 w-5" />
              <span className="hidden lg:inline">AppDirect </span>
              <span className="font-extrabold ml-2">AI</span>
            </Link>
            <ul className="hidden md:flex justify-between md:gap-6 lg:gap-14 py-2 ">
              <li>
                <Link href="/landing/features">How it works</Link>
              </li>
              <li>
                <Link href="/landing/solutions">Solutions</Link>
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
            <Link href="/login" className="py-2">
              Login
            </Link>
            <Link href="/signup" className="ml-4 lg:ml-10 px-4 py-2 bg-sky">
              Sign up
            </Link>
          </div>
        </nav>
      </div>
    </div>
  );
};

export default LandingNav;
