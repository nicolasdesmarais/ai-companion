import Link from "next/link";
import { AppdirectSvg } from "./svg/appdirect-svg";

const LandingFooter = () => {
  const currentYear = new Date().getFullYear();
  return (
    <div className="flex text-white bg-navy justify-center w-full py-8">
      <nav className="flex my-4 w-[1144px] justify-between">
        <div className="w-full mx-8">
          <div className="flex-col lg:flex-row flex">
            <div className="flex mr-44 mb-8">
              <AppdirectSvg className="h-5 w-5" fill="white" />
              AppDirect <span className="font-extrabold ml-2">AI</span>
            </div>
            <div className="flex flex-wrap gap-6 lg:gap-8">
              <div className="w-32">
                <div className="font-bold">PRODUCT</div>
                <ul className="flex flex-col justify-between gap-4 py-2 font-light">
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
                </ul>
              </div>
              <div className="w-32">
                <div className="font-bold">RESOURCES</div>
                <ul className="flex flex-col justify-between gap-4 py-2 font-light">
                  <li>
                    <Link href="/landing/resources#tour">Tutorials</Link>
                  </li>
                  <li>
                    <Link href="/landing/resources#guides">References</Link>
                  </li>
                  <li>
                    <Link href="/landing/resources#faq">FAQ</Link>
                  </li>
                </ul>
              </div>
              <div className="w-32">
                <div className="font-bold">LEGAL</div>
                <ul className="flex flex-col justify-between gap-4 py-2 font-light">
                  <li>
                    <Link href="/landing/privacy">Privacy Notice</Link>
                  </li>
                  <li>
                    <Link href="/landing/terms">Terms of Use</Link>
                  </li>
                </ul>
              </div>
              <div className="w-32">
                <div className="font-bold">SOCIAL</div>
                <ul className="flex flex-col justify-between gap-4 py-2 font-light">
                  <li>
                    <Link
                      href="https://www.linkedin.com/company/appdirect"
                      target="_blank"
                    >
                      LinkedIn
                    </Link>
                  </li>
                  <li>
                    <Link
                      href="https://www.instagram.com/appdirect/"
                      target="_blank"
                    >
                      Instagram
                    </Link>
                  </li>
                </ul>
              </div>
              <div className="w-32">
                <Link
                  href="https://www.appdirect.com/about"
                  target="_blank"
                  className="font-bold"
                >
                  COMPANY
                </Link>
              </div>
            </div>
          </div>
          <div className="flex justify-between w-full mt-8 items-center">
            <div>{`AppDirect ©${currentYear}`}</div>
            <Link href="/signup" className="ml-10 px-6 py-2 bg-white text-navy">
              Sign up
            </Link>
          </div>
        </div>
      </nav>
    </div>
  );
};

export default LandingFooter;
