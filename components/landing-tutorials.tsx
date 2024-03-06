"use client";
import { cn } from "@/src/lib/utils";
import { useState } from "react";

const steps = [
  {
    name: "1",
    src: "/create-demo.mp4",
    description: (
      <div>
        <b>Configure</b> with your own instructions and data to make fit for
        purpose AI applications.
      </div>
    ),
  },
  {
    name: "2",
    src: "/create-demo.mp4",
    description: (
      <div>
        <b>Personalize</b> with an avatar and personality settings controlling
        how creative and friendly or serious the AI is.
      </div>
    ),
  },
  {
    name: "3",
    src: "/create-demo.mp4",
    description: (
      <div>
        <b>Share</b> with with coworkers, friends and family and create a
        profile to help others know how to interact with your AI.
      </div>
    ),
  },
];

const LandingTutorials = () => {
  const [selectedStep, setSelectedStep] = useState(steps[0]);
  return (
    <div className="flex flex-col items-center mb-14 mt-20">
      <h3 className="text-3xl font-bold mb-16">Create apps without code</h3>
      <div className="flex">
        <ul className="flex flex-col justify-evenly items-center mr-8 my-10 border-l w-80">
          {steps.map((step, index) => (
            <li
              key={`ai-button-${index}`}
              className={cn(
                "cursor-pointer pl-4 font-bold",
                selectedStep.name === step.name && "border-l-8 border-lime pl-2"
              )}
              onClick={() => setSelectedStep(step)}
            >
              <div
                className={cn(
                  "p-4 w-8",
                  selectedStep.name === step.name && "bg-lime"
                )}
              >
                {step.name}
              </div>
              <div>{step.description}</div>
            </li>
          ))}
        </ul>
        <div className="m-8 py-8">
          <div className="mb-4">Step {selectedStep.name}</div>
          <div className="bg-white drop-shadow-lg">
            <video width="640" height="420" preload="none" autoPlay loop muted>
              <source src={selectedStep.src} type="video/mp4" />
            </video>
          </div>
        </div>
      </div>
    </div>
  );
};

export default LandingTutorials;
