import { cn } from "@/src/lib/utils";
import Link from "next/link";
import { MobileNav } from "./landing-mobile-nav";
import { AppdirectSvg } from "./svg/appdirect-svg";

interface Props {
  transparent?: boolean;
}

const LandingNav = ({ transparent }: Props) => {
  return (
    <div className="fixed w-full pr-4 z-10">
      <div
        className={cn(
          "flex text-navy justify-center w-full",
          transparent ? "" : "bg-white"
        )}
      >
        <nav className="flex m-4 w-[1144px] justify-between ">
          <div className="flex">
            <MobileNav />
            <Link
              href="/landing2/home"
              className="flex items-center md:mr-10 lg:mr-20"
            >
              <AppdirectSvg className="h-5 w-5" />
              AppDirect <span className="font-extrabold ml-2">AI</span>
            </Link>
            <ul className="hidden md:flex justify-between md:gap-8 lg:gap-14 py-2">
              <li>
                <Link href="/landing2/features">How it works</Link>
              </li>
              <li>
                <Link href="/landing2/solutions">Solutions</Link>
              </li>
              <li>
                <Link href="/landing2/pricing">Pricing</Link>
              </li>
              <li>
                <Link href="/landing2/enterprise">Enterprise</Link>
              </li>
              <li>
                <Link href="/landing2/resources">Resources</Link>
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
