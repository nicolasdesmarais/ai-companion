"use client";
import { AITeaser } from "@/components/ai-teaser";
import { WaitListForm } from "@/components/wait-list-form";
import { Button } from "@/components/ui/button";
import Image from "next/image";
import { ElementRef, useRef } from "react";
import Link from "next/link";

const LandingPage = () => {
  const waitlist = useRef<ElementRef<"div">>(null);
  return (
    <div className="bg-black flex flex-col">
      <div className="blue-bg max-h-[785px] h-screen pb-16 overflow-hidden flex flex-col">
        <div className="text-white/30 z-10 flex flex-row-reverse pt-4 pr-4 lg:mx-auto lg:w-[970px] lg:-mb-8">
          <Link href="/sign-in">Beta Login</Link>
        </div>
        <div className="flex justify-center items-center">
          <div>
            <div className="flex items-center justify-center border-b-2 border-white pb-4 mb-4">
              <Image
                src="/AppDirect-Mark_White.png"
                alt="AppDirect Logo"
                width="64"
                height="64"
                className="mt-5"
              />
              <span className="mt-5 ml-2 leading-none tracking-tight text-4xl text-white">
                AppDirect
              </span>
            </div>
            <div className="text-3xl font-bold">AI MARKETPLACE</div>
          </div>
        </div>
        <div className="flex justify-center">
          <div className="mt-40 mx-16 w-96 text-center lg:text-left">
            <h2
              className="me-4 mb-4 font-extrabold leading-none tracking-tight text-4xl text-white"
              title="AI made simple"
            >
              Lets add AI bots to your team
            </h2>
            <div>
              Transform AI bot ideas into reality without needing any coding
              skills.
            </div>
            <Button
              className="mt-5 rounded-none w-full"
              type="button"
              variant="ring"
              size="lg"
              onClick={() => {
                if (waitlist.current) {
                  (waitlist.current as any).scrollIntoView({
                    behavior: "smooth",
                  });
                }
              }}
            >
              Sign up to get notified
            </Button>
          </div>
          <AITeaser className="mt-20 hidden lg:block" />
        </div>
      </div>

      <div className="flex flex-col lg:flex-row justify-center p-8 lg:p-14 items-center lg:items-start">
        <div className="md:m-8 lg:w-96 space-y-4">
          <h3 className="text-3xl font-bold">Browse the AI Marketplace</h3>
          <div>
            Explore the rich catalog of AI&apos;s for everything from data
            analytics to legal support to marketing content.
          </div>
          <div>
            Search by category to quickly access the AI for your specific needs.
          </div>
        </div>
        <Image
          src="/browse_screenshot.jpg"
          alt="Browse Page"
          width="512"
          height="377"
          className="mt-5 shadow-glow"
        />
      </div>

      <div className="blue-bg flex flex-col lg:flex-row justify-center p-8 lg:p-14 items-center lg:items-start">
        <Image
          src="/datasources_screenshot.jpg"
          alt="Create AI Page"
          width="512"
          height="360"
          className="mt-5 shadow-glow"
        />
        <div className="md:ml-8 mt-8 lg:w-[395px] space-y-4 text-left lg:text-right">
          <h3 className="text-3xl font-bold">Create your own AI</h3>
          <div>
            Create custom AIs for yourself, your team, or your customers in
            under 30 minutes. No coding required.
          </div>
          <div>
            Whether you&apos;re a seasoned developer or a newcomer to the AI
            scene, our platform provides a guided user-friendly experience to
            create and manage AIs.
          </div>
        </div>
      </div>

      <div className="flex flex-col lg:flex-row justify-center p-8 lg:p-14 items-center lg:items-start">
        <div className="md:m-8 lg:w-96 space-y-4">
          <h3 className="mt-5 text-3xl font-bold">Interact with AIs</h3>
          <div>
            Chat with AIs to ask broad questions that help you summarize complex
            data sets or dig into the details quickly with specific queries.
          </div>
          <div>
            AIs learn what is most helpful for you with every interaction.
          </div>
        </div>
        <Image
          src="/interact_screenshot.jpg"
          alt="Chat Page"
          width="512"
          height="360"
          className="mt-5 shadow-glow"
        />
      </div>

      <div className="blue-bg flex flex-col lg:flex-row justify-center p-8 lg:p-14 items-center lg:items-start">
        <Image
          src="/share_screenshot.jpg"
          alt="Chat Page"
          width="512"
          height="377"
          className="mt-5 shadow-glow"
        />
        <div className="md:ml-8 mt-4 lg:w-[400px] space-y-4 text-left lg:text-right">
          <h3 className="mt-5 text-3xl font-bold">Share with confidence</h3>
          <div>
            Sharing your AI bot securely is paramount. AppDirect delivers robust
            enterprise-grade access control.
          </div>
          <div>
            You can specify who can interact with your bot and to what extent,
            safeguarding sensitive information and ensuring proper usage.
          </div>
        </div>
      </div>

      <div
        className="flex justify-center items-center flex-col pb-40"
        ref={waitlist}
      >
        <div className="text-3xl font-bold mt-16 mb-4 text-ring">
          AI MARKETPLACE
        </div>
        <div className="text-xl font-bold mb-16">Coming Soon...</div>
        <div>
          <div className="m-4">
            <h2 className="mt-5 text-2xl font-bold">Interested?</h2>
            <h2 className="">
              Be the first on the list when AppDirect AI becomes available.
            </h2>
          </div>
          <WaitListForm />
        </div>
        <div className="text-center">
          <a
            href="mailto:prforappdirect@bospar.com"
            className="text-ring cursor-pointer"
          >
            For press inquiries, please contact prforappdirect@bospar.com.
          </a>
        </div>
        <div className="flex items-center justify-center pb-4 mb-4 mt-16">
          <Image
            src="/AppDirect-Mark_White.png"
            alt="AppDirect Logo"
            width="64"
            height="64"
            className="mt-5"
          />
          <span className="mt-5 ml-2 text-3xl">AppDirect</span>
        </div>
      </div>
    </div>
  );
};

export default LandingPage;
