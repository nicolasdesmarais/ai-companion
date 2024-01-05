import Image from "next/image";
import { AITeaser } from "@/components/ai-teaser";
import Link from "next/link";

export function Avatar() {
  return;
}

const AuthLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <div className="blue-bg flex flex-col h-screen md:h-full md:overflow-clip">
      <Link href="/">
        <div className="flex justify-center items-center flex-col">
          <Image
            src="/AppDirect-Mark_White.png"
            alt="AppDirect Logo"
            width="64"
            height="64"
            className="mt-5"
          />
          <h1 className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
            AppDirect AI
          </h1>
        </div>
      </Link>
      <div className="flex justify-center h-full my-16">
        <div className="w-[500px] hidden md:block">
          <h2 className="me-4 mb-4 font-extrabold leading-none tracking-tight text-gray-900 text-4xl dark:text-white">
            Access for the Closed Beta
          </h2>
          <div>
            AppDirect AI is currently in a Closed Beta. If you have not received
            explicit instruction that you can access the product, please return
            to the homepage and sign up to be notified when we open up for more
            users.
          </div>
          <Image
            src="/teaser-splash.png"
            alt="Splash Image of AppDirect AIs"
            width="517"
            height="705"
            className="mt-5"
          />
        </div>
        <div className="w-[500px] flex flex-col items-center">{children}</div>
      </div>
    </div>
  );
};

export default AuthLayout;
