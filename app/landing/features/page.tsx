import LandingCTA from "@/components/landing-cta";
import LandingFooter from "@/components/landing-footer";
import LandingLLM from "@/components/landing-llm";
import LandingNav from "@/components/landing-nav";
import { ArrowRight } from "lucide-react";
import Image from "next/image";
import Link from "next/link";

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8] ">
        <div className="lg:w-[1140px] mx-4">
          <h2 className="text-5xl font-bold">How it works</h2>
          <div className="lg:w-[700px] mt-10 text-lg">
            Transform your AI app ideas into reality without needing any coding
            skills. Unlock innovation and productivity for you, your team, and
            your customers.
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center mt-20 mb-12 mx-4 lg:mx-0">
        <div className="flex flex-col-reverse lg:flex-row">
          <div className="lg:w-[460px] gap-8 flex flex-col mt-10 lg:mt-0">
            <div>Create</div>
            <div className="text-3xl font-extrabold lg:w-[300px]">
              Custom AI apps, created in minutes
            </div>
            <ul className="list-disc gap-4 flex flex-col ml-4">
              <li>
                <b>Advanced data ingestion options:</b> AppDirect AI provides
                more built-in choices than any other solution to define your
                app&nbsp;s data sources (files, drive folders, and websites) and
                schedule automated data updates.
              </li>
              <li>
                <b>App personality optimization:</b> Customize your AI app to
                speak with users in an appropriate language style for the task.
              </li>
              <li>
                <b>Secure environment:</b> Build your AI app on your proprietary
                data sets in a secure environment without worry.
              </li>
            </ul>
            <Link href="/sign-up" className="py-2 text-royal">
              Start building
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
          <div className="lg:bg-gradient1 flex justify-center items-center lg:p-16 lg:w-[560px] lg:w-[524px] lg:ml-20">
            <Image
              src="/datasources_screenshot.jpg"
              alt="AI Data Source Screenshot"
              width="512"
              height="360"
            />
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center my-12 py-16 px-4 bg-[#F8F8F8]">
        <div className="flex flex-col lg:flex-row">
          <div className="lg:bg-gradient2 flex items-center lg:p-16 lg:w-[560px] lg:w-[524px] justify-center">
            <Image
              src="/chat-screenshot.png"
              alt="AI Chat Screenshot"
              width="430"
              height="293"
            />
          </div>
          <div className="lg:w-[460px] gap-8 flex flex-col lg:ml-20 mt-10 lg:mt-0">
            <div>Use</div>
            <div className="text-3xl font-extrabold lg:w-[300px]">
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
            <div className="text-3xl font-extrabold lg:w-[300px]">
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
            <div className="text-3xl font-extrabold lg:w-[360px]">
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

export default LandingFeatures;
