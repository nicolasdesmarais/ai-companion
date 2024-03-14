import { cn } from "@/src/lib/utils";
import Image from "next/image";

interface Props {
  className?: string;
}

const LandingLLM = ({ className }: Props) => {
  return (
    <div
      className={cn(
        "flex flex-col items-center mt-20 mb-12 gap-8 mx-4",
        className
      )}
    >
      <h2 className="text-3xl font-extrabold">Choose your LLM</h2>
      <div className="mx-8 sm:mx-0 sm:w-[410px] lg:w-[820px] text-center">
        Choose the LLM provider most suitable for the task for each app you
        create. You can also create your own model with the help from our
        partner, Ivado Labs.
      </div>
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8 md:gap-12 flex-col md:flex-row">
        <div className="bg-[#F0F0F0] shadow-lg flex items-center w-64 justify-center h-20">
          <Image
            src="/meta.png"
            alt="Meta Logo"
            width="131"
            height="51"
            className="w-32"
          />
        </div>
        <div className="bg-[#F0F0F0] shadow-lg flex items-center w-64 justify-center h-20">
          <Image
            src="/anthropic.png"
            alt="Anthropic Logo"
            width="177"
            height="21"
          />
        </div>
        <div className="bg-[#F0F0F0] shadow-lg flex items-center w-64 justify-center h-20">
          <Image src="/openai.png" alt="OpenAI Logo" width="141" height="39" />
        </div>
        <div className="bg-[#F0F0F0] shadow-lg flex items-center w-64 justify-center h-20">
          <Image src="/cohere.png" alt="Cohere Logo" width="162" height="28" />
        </div>
      </div>
    </div>
  );
};

export default LandingLLM;
