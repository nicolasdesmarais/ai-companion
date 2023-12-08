import Image from "next/image";
import { AITeaser } from "@/components/ai-teaser";

export function Avatar() {
  return;
}

const AuthLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <div className="blue-bg flex flex-col md:h-full md:overflow-hidden">
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
      <div className="flex justify-center h-full my-16">
        <div className="w-[500px] hidden md:block">
          <h2 className="me-4 mb-4 font-extrabold leading-none tracking-tight text-gray-900 text-4xl dark:text-white">
            Let AI change the way you work.
          </h2>
          <div>
            AppDirect gives you the ability to access AIs that help you be more
            successful in work and life. You can browse our catalog of community
            made AIs or create your own!
          </div>
          <AITeaser />
        </div>
        <div className="w-[500px] flex flex-col items-center">{children}</div>
      </div>
    </div>
  );
};

export default AuthLayout;
