import { ArrowRight } from "lucide-react";
import Image from "next/image";

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
    description: "Customer support",
    src: "/michael.png",
  },
  {
    name: "Yvonne",
    description: "Marketing",
    src: "/yvonne.png",
  },
];

const LandingStartChat = () => {
  const selectedAi = ais[0];
  return (
    <div className="flex flex-col items-center">
      <h3 className="text-3xl font-bold mb-8">Start chatting with AIs</h3>
      <div className="flex">
        <ul className="flex flex-col justify-evenly items-center">
          {ais.map((ai) => (
            <li>
              <Image
                src={ai.src}
                alt={`{ai.name} AI`}
                width="70"
                height="75"
                className="mb-2"
              />
              Meet {ai.name}
            </li>
          ))}
        </ul>
        <div className="m-8 p-4 w-[800px] bg-white drop-shadow-lg">
          <div>{selectedAi.header}</div>
          <div>{selectedAi.bold}</div>
          <div className="flex flex-wrap justify-evenly m-8">
            {selectedAi.questions?.map((question) => (
              <div className="bg-lime p-4 m-2 rounded-lg">{question}</div>
            ))}
          </div>
          <div>
            Start chatting with {selectedAi.name}
            <ArrowRight className="inline-block w-4 h-4 ml-2" />
          </div>
        </div>
      </div>
    </div>
  );
};

export default LandingStartChat;
