"use client";
import { AI } from "@prisma/client";
import React from "react";
import { useAIProfile } from "@/hooks/use-ai-profile";
import { StarRating } from "./star-rating";
import Image from "next/image";
import { X } from "lucide-react";
import { Button } from "./ui/button";

interface Props {
  ai: AI & { profile: any };
  rating?: any;
}

export const AIProfile = ({ ai, rating }: Props) => {
  const { isOpen, onClose } = useAIProfile();
  if (!isOpen) return null;
  return (
    <div className="bg-accent/30 px-6 space-y-4 w-2/3 w-full overflow-auto ml-1">
      <div className="absolute top-4 right-4">
        <Button onClick={onClose} variant="ghost" size="icon" type="button">
          <X className="h-6 w-6" />
        </Button>
      </div>
      <div className="pt-1">
        <Button variant="link" size="sm" type="button">
          Profile
        </Button>
        <Button variant="link" size="sm" type="button">
          Specifications
        </Button>
        <Button variant="link" size="sm" type="button">
          Reviews
        </Button>
      </div>
      <div className="flex">
        <div className="relative w-40 h-40 mr-4 shrink-0">
          <Image
            fill
            alt="AI Profile Image"
            src={ai.src}
            className="rounded-lg object-cover"
          />
        </div>
        <div className="flex flex-col justify-between">
          <div>
            <div className="text-2xl font-bold">{ai.name}</div>
            <div className="text-sm text-muted-foreground">
              By {ai.userName}
            </div>
            <div className="mt-1">{ai.description}</div>
          </div>
          <StarRating
            value={Math.round(rating.averageRating)}
            count={rating.ratingCount}
            className=""
          />
        </div>
      </div>
      <div className="text-3xl font-bold">{ai.profile?.headline}</div>
      <div>{ai.profile?.description}</div>
      <div className="text-3xl font-bold">Features</div>
      {ai.profile?.features.map((feature: any, index: number) => (
        <div key={`feature-${index}`}>
          <div>{feature.title}</div>
          <div>{feature.description}</div>
        </div>
      ))}
      <div className="text-3xl font-bold">Training Specifications</div>
      {ai.profile?.showCharacter && <div>Character</div>}

      {ai.profile?.showPersonality && <div>Personality</div>}

      {ai.profile?.showTraining && <div>showTraining</div>}

      <div className="text-3xl font-bold">Reviews</div>
    </div>
  );
};
