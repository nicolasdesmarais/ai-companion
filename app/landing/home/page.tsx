"use client";
import LandingNav from "@/components/landing-nav";
import Image from "next/image";

const LandingHome = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />
      <div className="max-h-[785px] h-screen pb-16 overflow-hidden flex flex-col">
        <div className="flex justify-center">
          <div className="mt-40 mx-16 w-96 text-center lg:text-left">
            <h2
              className="me-4 mb-4 font-extrabold leading-none tracking-tight text-4xl"
              title="AI made simple"
            >
              AppDirect AI Marketplace & Creation Studio
            </h2>
            <div>
              Create custom AIs for yourself, your team, or your customers in
              under 30 minutes. No coding required.
            </div>
          </div>
          <Image
            src="/browse_screenshot.jpg"
            alt="Browse Page"
            width="512"
            height="377"
            className="mt-20 shadow-glow"
          />
        </div>
      </div>

      <div className="flex flex-col lg:flex-row justify-center p-8 lg:p-14 items-center lg:items-start">
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

      <div className="flex flex-col lg:flex-row justify-center p-8 lg:p-14 items-center lg:items-start">
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
    </div>
  );
};

export default LandingHome;
