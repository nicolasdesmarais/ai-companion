import LandingCTA from "@/components/landing-cta";
import LandingFooter from "@/components/landing-footer";
import LandingLLM from "@/components/landing-llm";
import LandingNav from "@/components/landing-nav";
import { ArrowRight } from "lucide-react";
import Image from "next/image";
import Link from "next/link";

const Solutions = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8] ">
        <div className="lg:w-[1140px] mx-4">
          <h2 className="text-5xl font-bold">Solutions</h2>
          <div className="lg:w-[700px] mt-10 text-lg">
            Streamline your daily tasks with the help of your own custom-fit AI
            app. Companies from all industries can use AI to boost productivity
            and efficiency.
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center mt-20 mb-12 mx-4 lg:mx-0">
        <div className="flex flex-col-reverse lg:flex-row justify-between">
          <div className="lg:w-[400px] gap-8 flex flex-col mt-10 lg:mt-0">
            <svg
              width="72"
              height="72"
              viewBox="0 0 72 73"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M40.2301 37.6501C46.1101 37.6501 50.8501 42.4201 50.8501 48.2701V66.5401H29.5801V48.2701C29.5801 42.3901 34.3501 37.6501 40.2001 37.6501H40.2301Z"
                fill="#ABE7FF"
              />
              <path
                d="M60.06 48.54C63.42 48.54 66.12 51.27 66.12 54.6V66.54H54V54.6C54 51.24 56.73 48.54 60.06 48.54Z"
                fill="#ABE7FF"
              />
              <path
                d="M17.9999 48.54C21.3599 48.54 24.0599 51.27 24.0599 54.6V66.54H11.9399V54.6C11.9399 51.24 14.6699 48.54 17.9999 48.54Z"
                fill="#ABE7FF"
              />
              <path
                d="M6 52.83V66.54H9.42V52.26C9.78 48.63 12.72 45.99 16.29 45.99H16.35C16.44 44.82 16.62 43.68 16.89 42.57C11.04 42.24 6 46.98 6 52.83Z"
                fill="#011B58"
              />
              <path
                d="M36 6.54004C29.37 6.54004 24 11.91 24 18.54C24 25.17 29.37 30.54 36 30.54C42.63 30.54 48 25.17 48 18.54C48 11.91 42.63 6.54004 36 6.54004ZM36 27.12C31.26 27.12 27.42 23.28 27.42 18.54C27.42 13.8 31.26 9.96004 36 9.96004C40.74 9.96004 44.58 13.8 44.58 18.54C44.58 23.28 40.74 27.12 36 27.12Z"
                fill="#011B58"
              />
              <path
                d="M16.5 24.54C12.36 24.54 9 27.9 9 32.04C9 36.18 12.36 39.54 16.5 39.54C20.64 39.54 24 36.18 24 32.04C24 27.9 20.64 24.54 16.5 24.54ZM16.5 35.79C14.43 35.79 12.75 34.11 12.75 32.04C12.75 29.97 14.43 28.29 16.5 28.29C18.57 28.29 20.25 29.97 20.25 32.04C20.25 34.11 18.57 35.79 16.5 35.79Z"
                fill="#011B58"
              />
              <path
                d="M55.5 24.54C51.36 24.54 48 27.9 48 32.04C48 36.18 51.36 39.54 55.5 39.54C59.64 39.54 63 36.18 63 32.04C63 27.9 59.64 24.54 55.5 24.54ZM55.5 35.79C53.43 35.79 51.75 34.11 51.75 32.04C51.75 29.97 53.43 28.29 55.5 28.29C57.57 28.29 59.25 29.97 59.25 32.04C59.25 34.11 57.57 35.79 55.5 35.79Z"
                fill="#011B58"
              />
              <path
                d="M35.9999 33.54C27.8099 33.54 21.1499 40.2 21.1499 48.39V66.54H24.4499V47.76C24.8099 41.61 29.8799 36.84 35.9999 36.84H36.6299C42.7799 37.2 47.5499 42.27 47.5499 48.39V66.54H50.8499V48.39C50.8499 40.2 44.1899 33.54 35.9999 33.54Z"
                fill="#011B58"
              />
              <path
                d="M55.5 42.54C55.77 43.65 55.95 44.79 56.04 45.96H56.67C60.3 46.32 62.94 49.26 62.94 52.83V66.54H66.36V52.83C66.36 46.95 61.38 42.24 55.47 42.57L55.5 42.54Z"
                fill="#011B58"
              />
            </svg>
            <div className="text-3xl font-bold ">AI for Human Resources</div>
            <div>
              Automate HR workflows while improving employee experience.
            </div>
            <ul className="list-disc gap-4 flex flex-col ml-4">
              <li>
                <b>Automate onboarding:</b> Load your company&apos;s process and
                onboarding training information into your very own AI app for a
                consistent employee experience.
              </li>
              <li>
                <b>Analyze employee engagement:</b> One organization used their
                AI to analyze their employees&apos; culture amp data to better
                understand the results, quickly.
              </li>
              <li>
                <b>Provide 24/7 access to HR support:</b> Quickly respond to
                employees&apos; HR-related queries, regardless of time zone.
              </li>
            </ul>
            <Link href="/sign-up" className="py-2 text-royal">
              Start creating
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
          <div className="pl-16">
            <Image
              src="/hr-chat.png"
              alt="AI HR Chat Screenshot"
              width="663"
              height="627"
            />
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center my-12 py-16 px-4 bg-[#F8F8F8]">
        <div className="flex flex-col lg:flex-row">
          <div className="lg:bg-gradient2 bg-left-top flex items-center lg:p-16 lg:w-[560px] lg:w-[524px] justify-center">
            <Image
              src="/chat-screenshot.png"
              alt="AI Chat Screenshot"
              width="430"
              height="293"
            />
          </div>
          <div className="lg:w-[460px] gap-8 flex flex-col lg:ml-20 mt-10 lg:mt-0">
            <div>Use</div>
            <div className="text-3xl font-bold lg:w-[300px]">
              Boost productivity-for everyone
            </div>
            <ul className="list-disc gap-4 flex flex-col ml-4">
              <li>
                <b>Thought partner:</b> Use your AI to help you get to the right
                answer, faster. Ask for advice on blind spots, get help editing
                a proposal, or learn how to align your calendar to your
                priorities.
              </li>
              <li>
                <b>Quickly pull insights:</b> Summarize large documents or data
                sets, pull themes to help you build a business case, or quickly
                cut through the noise to find answers.
              </li>
              <li>
                <b>Gain new knowledge:</b> Upload data and pull inquiries
                relevant to your specific needs to help you learn new skills,
                gain new insights, or challenge your assumptions..
              </li>
            </ul>
            <Link href="/public" className="py-2 text-royal">
              Start using
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center mt-20 mb-12 mx-4">
        <div className="flex flex-col-reverse lg:flex-row">
          <div className="lg:w-[460px] gap-8 flex flex-col mt-10 lg:mt-0">
            <div>Share</div>
            <div className="text-3xl font-bold lg:w-[300px]">
              Share apps with confidence
            </div>
            <ul className="list-disc gap-4 flex flex-col ml-4">
              <li>
                <b>Privacy first:</b> Sharing your AI bot securely is paramount.
                Whatever data you use to create your AI, rest assured that it
                will stay in the app and will never be used to train LLMs
                outside your organization.
              </li>
              <li>
                <b>AI-powered collaboration:</b> App sharing with colleagues
                gives teams a powerful tool to gain new insights, unlock
                innovation, and increase productivity.
              </li>
              <li>
                <b>Public sharing:</b> Sharing apps within your organization or
                publicly is easy and secure.
              </li>
            </ul>
            <Link href="/sign-up" className="py-2 text-royal">
              Start creating
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
          <div className="lg:bg-gradient2 flex items-center lg:p-16 lg:w-[560px] lg:w-[524px] lg:ml-20 justify-center">
            <Image
              src="/share-screenshot.png"
              alt="AI Share Screenshot"
              width="431"
              height="283"
            />
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center my-12 py-16 px-4 bg-[#F8F8F8]">
        <div className="flex flex-col lg:flex-row">
          <div className="lg:bg-gradient1 flex items-center lg:p-16 lg:w-[560px] lg:w-[524px] justify-center">
            <Image
              src="/browse_screenshot.jpg"
              alt="AI Browse Screenshot"
              width="512"
              height="377"
            />
          </div>
          <div className="lg:w-[460px] gap-8 flex flex-col lg:ml-20 mt-10 lg:mt-0">
            <div>Browse</div>
            <div className="text-3xl font-bold lg:w-[360px]">
              Explore marketplace of built-for-purpose AIs
            </div>
            <ul className="list-disc gap-4 flex flex-col ml-4 mt-6">
              <li>
                <b>Out of the box AIs:</b> Explore the rich catalog of AIs ready
                to use immediately.
              </li>
              <li>
                <b>Search by category:</b> Everything from data analytics to
                legal support to marketing content. You can also search for apps
                based on keywords.
              </li>
            </ul>
            <Link href="/public" className="py-2 mt-20 text-royal">
              Browse catalog
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
        </div>
      </div>

      <LandingLLM />

      <LandingCTA />

      <LandingFooter />
    </div>
  );
};

export default Solutions;
