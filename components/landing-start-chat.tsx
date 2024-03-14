"use client";
import { cn } from "@/src/lib/utils";
import { ArrowRight } from "lucide-react";
import Image from "next/image";
import { useState } from "react";

const ais = [
  {
    name: "Andrea",
    src: "/andrea.png",
    header: "Introducing Andrea, an AI specializing in sales enablement.",
    bold: "Ask her anything, such as...",
    questions: [
      "Quiz me on product features.",
      "Summarize this agenda.",
      "Rank my sales team based on annual revenue.",
      "Who are my top competitors?",
      "Improve the language on this blurb.",
      "Write a short description for a website.",
    ],
  },
  {
    name: "Michael",
    src: "/michael.png",
    header: "Introducing Michael, an AI specializing in Legal support.",
    bold: "Ask him anything, such as...",
    questions: [
      "Draft an employee contract.",
      "What I should include in an NDA?",
      "What is in an acquisition contract?",
      "LLC setup requirements?",
      "Compare these supplier SOWs according to ROI.",
    ],
  },
  {
    name: "Yvonne",
    src: "/yvonne.png",
    header: "Introducing Yvonne, an AI specializing in HR support.",
    bold: "Ask her anything, such as...",
    questions: [
      "How do I apply for parental leave?",
      "What holidays do we have off in the US?",
      "Where can I submit my travel expenses?",
      "When is open enrollment?",
      "Can you explain my stock options?",
      "How do I request time off?",
    ],
  },
];

const LandingStartChat = () => {
  const [selectedAi, setSelectedAi] = useState(ais[0]);

  return (
    <div className="flex flex-col items-center mb-14 mx-4">
      <h3 className="text-3xl font-bold mb-8">Start chatting with AIs</h3>
      <div className="flex flex-col md:flex-row">
        <ul className="flex flex-row md:flex-col justify-evenly items-center lg:mr-8 my-10 md:border-l">
          {ais.map((ai, index) => (
            <li
              key={`ai-button-${index}`}
              className={cn(
                "cursor-pointer pl-4 font-bold",
                ai.name === selectedAi.name && "border-l-8 border-lime pl-2"
              )}
              onClick={() => setSelectedAi(ai)}
            >
              <Image
                src={ai.src}
                alt={`{ai.name} AI`}
                width="70"
                height="75"
                className={cn("mb-2 rounded-md")}
              />
              Meet {ai.name}
            </li>
          ))}
        </ul>
        <div className="m-2 md:m-8 py-8 px-4 md:px-12 md:w-[500px] lg:w-[700px] xl:w-[900px] bg-white drop-shadow-lg">
          <div>{selectedAi.header}</div>
          <div className="font-bold">{selectedAi.bold}</div>
          <div className="flex flex-wrap justify-evenly m-2 md:m-8 min-h-96 lg:min-h-72">
            {selectedAi.questions?.map((question, index) => (
              <div
                key={`card-${index}`}
                className="bg-lime p-2 lg:p-4 m-2 rounded-lg font-mono md:whitespace-pre h-fit lg:h-14"
              >
                {question}
              </div>
            ))}
          </div>
          <div className="text-ring cursor-pointer">
            Start chatting with {selectedAi.name}
            <ArrowRight className="inline-block w-4 h-4 ml-2" />
          </div>
        </div>
      </div>
    </div>
  );
};

export default LandingStartChat;
