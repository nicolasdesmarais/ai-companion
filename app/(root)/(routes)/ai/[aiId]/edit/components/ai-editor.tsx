"use client";

import { Category, Prisma } from "@prisma/client";
import { AIForm } from "./ai-form";
import { useState } from "react";
import { cn } from "@/lib/utils";
import { AIKnowledge } from "./ai-knowledge";
import { AIPersonality } from "./ai-personality";

const extendedCompanion = Prisma.validator<Prisma.CompanionDefaultArgs>()({
  include: {
    knowledge: {
      include: {
        knowledge: true,
      },
    },
  },
});

type ExtendedCompanion = Prisma.CompanionGetPayload<typeof extendedCompanion>;

interface CompanionFormProps {
  categories: Category[];
  initialAi: ExtendedCompanion | null;
}

const tabs = ["Character", "Knowledge", "Personality"];

export const AIEditor = ({ categories, initialAi }: CompanionFormProps) => {
  const [activeTab, setActiveTab] = useState(0);
  const handleClick = (index: number) => setActiveTab(index);
  return (
    <div>
      <div className="flex h-full p-4 space-x-1 max-w-3xl mx-auto">
        {tabs.map((tab, index) => (
          <div
            className={cn(
              "flex grow bg-accent/50 justify-center first:rounded-l-lg last:rounded-r-lg py-4 text-ring transition",
              activeTab === index
                ? "bg-accent text-primary"
                : "cursor-pointer hover:bg-primary/10"
            )}
            key={index}
            onClick={() => handleClick(index)}
          >
            <div className="bg-secondary rounded-lg px-2 text-ring">
              {index + 1}
            </div>
            <div className="ml-2">{tab}</div>
          </div>
        ))}
      </div>
      <div>
        {activeTab === 0 && (
          <AIForm initialData={initialAi} categories={categories} />
        )}
        {activeTab === 1 && <AIKnowledge aiId={initialAi?.id} />}
        {activeTab === 2 && <AIPersonality initialAi={initialAi} />}
      </div>
    </div>
  );
};
