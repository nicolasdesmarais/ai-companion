"use client";
import { cn } from "@/src/lib/utils";
import { useState } from "react";

const steps = [
  {
    name: "One",
    src: "/configure-demo.mp4",
    title: "Configure",
    description:
      "with your own instructions and data to make fit for purpose AI applications.",
  },
  {
    name: "Two",
    src: "/personalize-demo.mp4",
    title: "Personalize",
    description:
      "with an avatar and personality settings controlling how creative and friendly or serious the AI is.",
  },
  {
    name: "Three",
    src: "/share-demo.mp4",
    title: "Share",
    description:
      "with coworkers, friends and family and create a profile to help others know how to interact with your AI.",
  },
];

const LandingTutorials = () => {
  const [selectedStep, setSelectedStep] = useState(steps[0]);
  return (
    <div className="flex flex-col items-center mb-14 mt-20 mx-4">
      <h3 className="text-3xl font-bold mb-4 lg:mb-16">
        Create apps without using code
      </h3>
      <div className="flex flex-col lg:flex-row min-h-[610px]">
        <ul className="flex flex-col justify-evenly md:items-center mr-8 my-10 lg:border-l lg:w-80">
          {steps.map((step, index) => (
            <li
              key={`step-button-${index}`}
              className={cn(
                "cursor-pointer pl-2 lg:pl-6 font-bold",
                selectedStep.name === step.name &&
                  "lg:border-l-8 border-lime lg:pl-4"
              )}
              onClick={() => setSelectedStep(step)}
            >
              <div
                className={cn(
                  "flex flex-col py-4 px-2 mb-2 hidden lg:flex",
                  selectedStep.name === step.name &&
                    "bg-lime w-12 items-center "
                )}
              >
                {index + 1}
              </div>
              <span className="lg:hidden bg-lime px-4 py-2">
                Step {step.name}
              </span>
              <h4 className="lg:hidden mt-8 text-xl">{step.title}</h4>
              <div className="lg:hidden text-sm font-light mt-4">
                {step.title} {step.description}
              </div>
              <div className="hidden lg:flex text-sm font-light mt-8 lg:mg-0">
                <div>
                  <b className="font-bold">{step.title}</b> {step.description}
                </div>
              </div>
              <div className="flex lg:hidden mt-8 mb-16">
                <video width="640" height="420" autoPlay loop muted playsInline>
                  <source src={step.src} type="video/mp4" />
                </video>
              </div>
            </li>
          ))}
        </ul>
        <div className="m-8 py-8 hidden lg:block">
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
          <div className="bg-white drop-shadow-lg hidden lg:flex">
            <video
              key={selectedStep.src}
              width="640"
              height="420"
              autoPlay
              loop
              muted
              playsInline
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
