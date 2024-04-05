"use client";
import { cn } from "@/src/lib/utils";
import { ArrowRight } from "lucide-react";
import Image from "next/image";
import Link from "next/link";
import { useState } from "react";

const ais = [
  {
    name: "Accounting Assistant",
    src: "/accounting-ai.png",
    link: "https://appdirect.ai/public/ai/836be20b-b6ca-4f3d-9191-70be800b7bea",
    header:
      "Introducing the Accounting Assistant, an AI created to help you with accounting and financial planning questions.",
    bold: "Ask it anything, such as...",
    questions: [
      "How should I prepare for my annual tax return?",
      "What do I need for tax deductions?",
      "How can I improve my personal financial planning?",
      "How to record my business expenses?",
      "Tax implications of hiring employees vs contractors?",
      "How to maximize tax deductions for my business?",
    ],
  },
  {
    name: "Legal Aid",
    src: "/legal-ai.png",
    link: "https://appdirect.ai/public/ai/2aea1d97-9ceb-4988-a412-2539a58b0858",
    header:
      "Introducing the Legal Aid, an AI created to support employees or business owners with any legal questions they have.",
    bold: "Ask it anything, such as...",
    questions: [
      "LLC vs. corporation vs. partnership?",
      "How to safeguard business with trademarks, patents, copyrights?",
      "Legalities for taking local business international?",
      "Safeguarding business from lawsuits?",
      "Legal implication in hiring contractors?",
      "Legal steps before starting a new business?",
      "Legal steps before starting a new business?",
    ],
  },
  {
    name: "HR Specialist",
    src: "/hr-ai.png",
    link: "https://appdirect.ai/public/ai/14112ef6-dd35-4b7a-b6ac-b71042a5416c",
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
                "cursor-pointer md:pl-4 font-bold w-20",
                ai.name === selectedAi.name &&
                  "border-b-8 md:border-b-0 md:border-l-8 border-lime md:pl-2"
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
          <div className="font-bold my-2">{selectedAi.bold}</div>
          <div className="flex flex-wrap justify-evenly m-2 md:m-4 min-h-[360px]">
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
