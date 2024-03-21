"use client";
import { cn } from "@/src/lib/utils";
import { Minus, Plus } from "lucide-react";
import { useState } from "react";

const faqs = [
  {
    question: "What is AppDirect AI?",
    answer: (
      <div>
        AppDirect AI is a marketplace where you can explore and use
        fit-for-purpose AIs that can be used immediately to augment your
        day-to-day tasks. You can also create your own in minutes using our
        Creator Studio and your own data.
      </div>
    ),
  },
  {
    question: "Will my data be shared?",
    answer: (
      <div>
        The proprietary datasets you upload to your AI app are stored on
        completely private, secure servers and will never be shared outside of
        your AI, so you can fearlessly upload and build apps with your data
        without worry.
      </div>
    ),
  },
  {
    question: "What does LLM-agnostic mean to me?",
    answer: (
      <div>
        With AppDirect AI, you are not limited to using one LLM (Large Language
        Model, such as GPT or Cohere) unlike other AI platforms. You choose your
        app&apos;s LLM based on what is most suited to your application needs,
        and you can modify it at any time. We currently support:
        <ul className="list-disc m-4">
          <li>GPT-4 (32K Context)</li>
          <li>GPT-3.5 (16K Context)</li>
          <li>GPT-4 Turbo w/Assistant API</li>
          <li>LLAMA2 13B Chat (4K Context)</li>
          <li>LLAMA2 70B Chat (4K Context)</li>
          <li>LLAVA 13B</li>
        </ul>
      </div>
    ),
  },
  {
    question: "How do I create an app?",
    answer: (
      <div>
        It&apos;s as easy as 1, 2, 3.
        <ol className="list-decimal m-4">
          <li>
            Sign up to be a creator—Join an existing organization or create your
            own
          </li>
          <li>
            Create your first app—Use the easy guided steps to get your app up
            and running in minutes
          </li>
          <li>
            Start innovating—Use your app and share it with others to foster
            collaboration, productivity, and innovation
          </li>
        </ol>
      </div>
    ),
  },
  {
    question: "Do I need to know how to code to create an app?",
    answer: (
      <div>
        <div>
          No coding required! Our AI creation tool takes you through simple
          steps to create your AI, with no coding involved.
        </div>
        <div>
          If you have complex AI requirements and think you&apos;ll need
          additional assistance, we can help you. Contact our Enterprise team
          for more information.
        </div>
      </div>
    ),
  },
  {
    question: "Do you have a free version?",
    answer: (
      <div>
        Yes! We offer both free and paid plans based on data usage. Check out
        our pricing plans.
      </div>
    ),
  },
  {
    question: "What is RAG?",
    answer: (
      <div>
        Retrieval-augmented generation (RAG) is a technique that enhances the
        accuracy and reliability of generative AI models. Every AI you create
        with AppDirect incorporates RAG to ensure the integrity and relevance of
        your app&apos;s responses.
      </div>
    ),
  },
  {
    question: "What sets the AppDirect AI apart?",
    answer: (
      <div>
        Our solution uniquely blends rapid no-code app creation, our data
        privacy and protection guarantee, and advanced data ingestion options
        that let you dive into AI without the typical risks. With AppDirect AI,
        you can easily share your apps and find out-of-the-box AI apps from
        leading providers.
      </div>
    ),
  },
  {
    question: "I have another question",
    answer: (
      <div>
        We&apos;re happy to help! Email us at support@appdirectai.com if you
        have any questions.
      </div>
    ),
  },
];

interface Props {
  className?: string;
}

const LandingFAQ = ({ className }: Props) => {
  const [visible, setVisible] = useState([true]);
  const toggle = (index: number) => {
    setVisible((prev) => {
      const newVisible = [...prev];
      newVisible[index] = !newVisible[index];
      return newVisible;
    });
  };
  return (
    <div
      className={cn(
        "flex flex-col items-center mt-20 mb-12 gap-8 mx-4",
        className
      )}
    >
      <h2 className="text-3xl font-bold">FAQs</h2>
      <div className="flex flex-col justify-center items-center">
        {faqs.map((faq, index) => (
          <div
            key={`faq-${index}`}
            className="w-full lg:w-[1140px] space-y-4 bg-[#FAF7F7] p-8 my-4"
          >
            <div
              className="flex justify-between cursor-pointer"
              onClick={() => toggle(index)}
            >
              <h3 className="text-xl font-bold">{faq.question}</h3>
              {visible[index] ? (
                <Minus className="w-6 h-6" />
              ) : (
                <Plus className="w-6 h-6" />
              )}
            </div>
            {visible[index] && <div>{faq.answer}</div>}
          </div>
        ))}
      </div>
    </div>
  );
};

export default LandingFAQ;
