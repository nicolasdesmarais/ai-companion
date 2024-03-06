"use client";
import { cn } from "@/src/lib/utils";
import { useState } from "react";

const steps = [
  {
    name: "1",
    src: "/andrea.png",
    description: (
      <div>
        <b>Configure</b> with your own instructions and data to make fit for
        purpose AI applications.
      </div>
    ),
  },
  {
    name: "2",
    src: "/michael.png",
    description: (
      <div>
        <b>Personalize</b> with an avatar and personality settings controlling
        how creative and friendly or serious the AI is.
      </div>
    ),
  },
  {
    name: "3",
    src: "/yvonne.png",
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
        <ul className="flex flex-col justify-evenly items-center mr-8 my-10 border-l">
          {steps.map((step, index) => (
            <li
              key={`ai-button-${index}`}
              className={cn(
                "cursor-pointer pl-4 font-bold",
                selectedStep.name === step.name && "border-l-8 border-lime pl-2"
              )}
              onClick={() => setSelectedStep(step)}
            >
              {step.name}
              <div>{step.description}</div>
            </li>
          ))}
        </ul>
        <div className="m-8 py-8 px-12 w-[900px] bg-white drop-shadow-lg">
          <div>Step {selectedStep.name}</div>
        </div>
      </div>
    </div>
  );
};

export default LandingTutorials;
