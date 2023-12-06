"use client";
import { AI } from "@prisma/client";
import React from "react";
import { useAIProfile } from "@/hooks/use-ai-profile";
import { StarRating } from "./star-rating";
import Image from "next/image";

interface Props {
  ai: AI & { profile: any };
  rating?: any;
}

export const AIProfile = ({ ai, rating }: Props) => {
  const { isOpen, onClose } = useAIProfile();
  if (!isOpen) return null;
  return (
    <div className="bg-accent/30">
      <div className="relative w-40 h-40">
        <Image
          fill
          alt="AI Profile Image"
          src={ai.src}
          className="rounded-lg object-cover"
        />
      </div>
      <div>{ai.name}</div>
      <div>By {ai.userName}</div>
      <div>{ai.description}</div>
      <StarRating
        value={Math.round(rating.averageRating)}
        count={rating.ratingCount}
        className=""
      />
      <div>{ai.profile?.headline}</div>
      <div>{ai.profile?.description}</div>
      <div>Features</div>
      {ai.profile?.features.map((feature: any, index: number) => (
        <div key={`feature-${index}`}>
          <div>{feature.title}</div>
          <div>{feature.description}</div>
        </div>
      ))}
      <div>Training Specifications</div>
      {ai.profile?.showCharacter && <div>Character</div>}

      {ai.profile?.showPersonality && <div>Personality</div>}

      {ai.profile?.showTraining && <div>showTraining</div>}

      <div>Reviews</div>
    </div>
  );
};
