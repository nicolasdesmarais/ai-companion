"use client";
import { cn } from "@/src/lib/utils";
import { ArrowRight } from "lucide-react";
import Image from "next/image";
import Link from "next/link";
import { useState } from "react";

const ais = [
  {
    name: "Writing Assistant",
    src: "/writing-ai.png",
    link: "https://appdirect.ai/public/ai/f55e4fc8-2d7b-47b0-b077-7ab6627b9ac3",
    header:
      "Introducing the Writing Assistant, an AI created to help edit your writing or provide information on proper grammar and spelling.",
    bold: "Ask it anything, such as...",
    questions: [
      "Can you proofread my essay?",
      "Can you check this report for errors?",
      "Refine this sentence.",
      "Can this story ending be improved, and how?",
      "Can you help write a supplier inquiry email?",
      "Please reduce this to 50 characters.",
    ],
  },
  {
    name: "Spanish Tutor",
    src: "/spanish-ai.png",
    link: "https://appdirect.ai/public/ai/1cd5ae8e-81a5-4f81-b9df-f005ab67f260",
    header:
      "Introducing the Spanish Tutor, an AI created to help people to speak Spanish if it's not their native language.",
    bold: "Ask it anything, such as...",
    questions: [
      `Can you provide common adjectives in Spanish for "happily"?`,
      `What is the difference between "ser" and "estar"?`,
      "How do I form the past tense in Spanish?",
      `How do I use "estar" in a sentence?`,
      "How is the subjunctive used in Spanish?",
      "How do you pronounce hello?",
    ],
  },
  {
    name: "HR Specialist",
    src: "/hr-ai.png",
    link: "https://appdirect.ai/public/ai/f55e4fc8-2d7b-47b0-b077-7ab6627b9ac3",
    header:
      "Introducing the HR Specialist, an AI created to represent general guidelines for United States HR policies and expectations.",
    bold: "Ask it anything, such as...",
    questions: [
      "Who is eligible under the FMLA and what are their rights?",
      "What are the standards for a legal interview process?",
      "What benefits are employers required to provide?",
      "What are wage and overtime guidelines?",
      "What laws apply for different states' employees?",
      "How should harassment complaints be handled?",
    ],
  },
];

const LandingStartChat = () => {
  const [selectedAi, setSelectedAi] = useState(ais[0]);

  return (
    <div className="flex flex-col items-center mb-14 mx-4">
      <h3 className="text-3xl font-bold mb-8">Start chatting with AIs</h3>
      <div className="flex flex-col md:flex-row">
        <ul className="flex flex-row md:flex-col justify-evenly lg:mr-8 my-10 md:border-l">
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
              {ai.name}
            </li>
          ))}
        </ul>
        <div className="m-2 md:m-8 py-8 px-4 md:px-12 md:w-[500px] lg:w-[700px] xl:w-[900px] bg-white drop-shadow-lg">
          <div>{selectedAi.header}</div>
          <div className="font-bold">{selectedAi.bold}</div>
          <div className="flex flex-wrap justify-evenly m-2 md:m-8 min-h-[435px]">
            {selectedAi.questions?.map((question, index) => (
              <div
                key={`card-${index}`}
                className="bg-lime p-2 lg:p-4 m-2 rounded-lg font-mono md:whitespace-pre h-fit lg:h-14"
              >
                {question}
              </div>
            ))}
          </div>
          <Link href={selectedAi.link}>
            <div className="text-ring cursor-pointer">
              Start chatting with {selectedAi.name}
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </div>
          </Link>
        </div>
      </div>
    </div>
  );
};

export default LandingStartChat;
