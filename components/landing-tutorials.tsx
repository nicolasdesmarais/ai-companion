"use client";
import { cn } from "@/src/lib/utils";
import { useState } from "react";

const steps = [
  {
    name: "1",
    src: "/configure-demo.mp4",
    description: (
      <div>
        <b className="font-bold">Configure</b> with your own instructions and
        data to make fit for purpose AI applications.
      </div>
    ),
  },
  {
    name: "2",
    src: "/personalize-demo.mp4",
    description: (
      <div>
        <b className="font-bold">Personalize</b> with an avatar and personality
        settings controlling how creative and friendly or serious the AI is.
      </div>
    ),
  },
  {
    name: "3",
    src: "/share-demo.mp4",
    description: (
      <div>
        <b className="font-bold">Share</b> with with coworkers, friends and
        family and create a profile to help others know how to interact with
        your AI.
      </div>
    ),
  },
];

const LandingTutorials = () => {
  const [selectedStep, setSelectedStep] = useState(steps[0]);
  console.log(selectedStep.src);
  return (
    <div className="flex flex-col items-center mb-14 mt-20">
      <h3 className="text-3xl font-bold mb-16">Create apps without code</h3>
      <div className="flex min-h-[610px]">
        <ul className="flex flex-col justify-evenly items-center mr-8 my-10 border-l w-80">
          {steps.map((step, index) => (
            <li
              key={`step-button-${index}`}
              className={cn(
                "cursor-pointer pl-6 font-bold",
                selectedStep.name === step.name && "border-l-8 border-lime pl-4"
              )}
              onClick={() => setSelectedStep(step)}
            >
              <div
                className={cn(
                  "flex flex-col py-4 px-2 mb-2",
                  selectedStep.name === step.name &&
                    "bg-lime w-12 items-center "
                )}
              >
                {step.name}
              </div>
              <div className="text-sm font-light">{step.description}</div>
            </li>
          ))}
        </ul>
        <div className="m-8 py-8">
          <ul className="flex justify-between">
            {steps.map((step, index) => (
              <li
                key={`tutorial-step-${index}`}
                onClick={() => setSelectedStep(step)}
                className={cn(
                  "cursor-pointer mb-4 px-16 py-2 font-bold",
                  selectedStep.name === step.name && "bg-lime"
                )}
              >
                <div className="">Step {step.name}</div>
              </li>
            ))}
          </ul>
          <div className="bg-white drop-shadow-lg">
            <video
              key={selectedStep.src}
              width="640"
              height="420"
              preload="none"
              autoPlay
              loop
              muted
            >
              <source src={selectedStep.src} type="video/mp4" />
            </video>
          </div>
        </div>
      </div>
    </div>
  );
};

export default LandingTutorials;
