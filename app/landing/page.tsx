import { AITeaser } from "@/components/ai-teaser";
import { LandingForm } from "@/components/landing-form";
import { Button } from "@/components/ui/button";
import Image from "next/image";

const LandingPage = () => {
  return (
    <div className="bg-black flex flex-col">
      <div className="blue-bg flex justify-center h-screen py-16">
        <div className="">
          <Image
            src="/AppDirect-Mark_White.png"
            alt="AppDirect Logo"
            width="64"
            height="64"
            className="mt-5"
          />
          <span className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
            AppDirect
          </span>
          <div>AI MARKETPLACE</div>
          <h2
            className="me-4 mb-4 font-extrabold leading-none tracking-tight text-gray-900 text-4xl dark:text-white"
            title="AI made simple"
          >
            Lets add AI bots to your team
          </h2>
          <div>
            Transform AI bot ideas into reality without needing any coding
            skills.
          </div>
          <Button className="mt-5 rounded-none" type="button">
            Sign up to get notified
          </Button>
        </div>
      </div>

      {/* <AITeaser /> */}

      <div className="flex justify-center items-center">
        <div>
          <h3 className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
            Browse the AI Marketplace
          </h3>
          <div>
            Explore the rich catalog of AI&apos;s for everything from data
            analytics to legal support to marketing content. Search by category
            to quickly access the AI for your specific needs.
          </div>
        </div>
        <Image
          src="/browse_screenshot.jpg"
          alt="Browse Page"
          width="1370"
          height="1010"
          className="mt-5"
        />
      </div>

      <div className="blue-bg flex justify-center items-center">
        <Image
          src="/datasources_screenshot.jpg"
          alt="Create AI Page"
          width="512"
          height="360"
          className="mt-5"
        />
        <div>
          <h3 className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
            Create your own AI
          </h3>
          <div>
            Create custom AIs for yourself, your team, or your customers in
            under 30 minutes. No coding required. Whether you&apos;re a seasoned
            developer or a newcomer to the AI scene, our platform provides a
            guided user-friendly experience to create and manage AIs.
          </div>
        </div>
      </div>

      <div className="flex justify-center items-center">
        <div>
          <h3 className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
            Interact with AIs
          </h3>
          <div>
            Chat with AIs to ask broad questions that help you summarize complex
            data sets or dig into the details quickly with specific queries. AIs
            learn what is most helpful for you with every interaction.
          </div>
        </div>
        <Image
          src="/interact_screenshot.jpg"
          alt="Chat Page"
          width="1357"
          height="955"
          className="mt-5"
        />
      </div>

      <div className="blue-bg flex justify-center items-center">
        <div>
          <h3 className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
            Share with confidence
          </h3>
          <div>
            Sharing your AI bot securely is paramount. AppDirect delivers robust
            enterprise-grade access control. You can specify who can interact
            with your bot and to what extent, safeguarding sensitive information
            and ensuring proper usage.
          </div>
        </div>
        <Image
          src="/share_screenshot.jpg"
          alt="Chat Page"
          width="512"
          height="377"
          className="mt-5"
        />
      </div>

      <div className="flex justify-center items-center flex-col">
        <Image
          src="/AppDirect-Mark_White.png"
          alt="AppDirect Logo"
          width="64"
          height="64"
          className="mt-5"
        />
        <h2 className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
          Interested?
        </h2>
        <h2 className="mt-4">
          Be first on the list when this becomes available.
        </h2>
        <LandingForm />
        <div>
          For press inquiries, please contact{" "}
          <a
            href="mailto:prforappdirect@bospar.com"
            className="text-ring cursor-pointer"
          >
            prforappdirect@bospar.com
          </a>
        </div>
      </div>
    </div>
  );
};

export default LandingPage;
