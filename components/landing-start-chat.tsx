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
        <div className="">
          <ul>
            {ais.map((ai) => (
              <li>
                <Image
                  src={ai.src}
                  alt={`{ai.name} AI`}
                  width="70"
                  height="75"
                />
                Meet {ai.name}
              </li>
            ))}
          </ul>
        </div>
        <div className="m-8">
          <div>{selectedAi.description}</div>
          <div>{selectedAi.bold}</div>
          {selectedAi.questions?.map((question) => (
            <div>{question}</div>
          ))}
          <div>Start chatting with {selectedAi.name}</div>
        </div>
      </div>
    </div>
  );
};

export default LandingStartChat;
