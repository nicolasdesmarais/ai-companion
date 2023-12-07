"use client";
import { AI } from "@prisma/client";
import React, { useEffect, useState } from "react";
import { useAIProfile } from "@/hooks/use-ai-profile";
import { StarRating } from "./star-rating";
import Image from "next/image";
import { X, Star } from "lucide-react";
import { Button } from "./ui/button";
import { StaticAIModelRepository } from "@/src/adapter-out/repositories/StaticAIModelRepository";
import axios from "axios";
import { StarSvg } from "./svg/star-svg";

const aiModelRepository = new StaticAIModelRepository();
interface Props {
  ai: AI & { profile: any };
  rating?: any;
}

export const AIProfile = ({ ai, rating }: Props) => {
  const { isOpen, onClose } = useAIProfile();
  const [dataSources, setDataSources] = useState<any[]>([]);

  useEffect(() => {
    const fetchDataSources = async () => {
      const response = await axios.get(`/api/v1/ai/${ai.id}/data-sources`);
      setDataSources(response.data.data);
    };
    if (ai.profile?.showTraining) {
      fetchDataSources();
    }
  }, []);

  if (!isOpen) return null;
  console.log(ai);
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
            <div className="text-xl font-bold">{ai.name}</div>
            <div className="text-sm text-muted-foreground">
              By {ai.userName}
            </div>
            <div className="mt-1 text-sm">{ai.description}</div>
          </div>
          <StarRating
            value={Math.round(rating.averageRating)}
            count={rating.ratingCount}
            className=""
          />
        </div>
      </div>
      <div className="text-2xl font-bold">{ai.profile?.headline}</div>
      <div className="text-sm">{ai.profile?.description}</div>
      <div className="text-2xl font-bold">
        <span className="border-b border-ring pb-1 pr-4">Features</span>
      </div>
      {ai.profile?.features.map((feature: any, index: number) => (
        <div key={`feature-${index}`}>
          <div className="text-xl font-bold mb-2">{feature.title}</div>
          <div className="text-sm">{feature.description}</div>
        </div>
      ))}
      <div className="text-2xl font-bold">
        <span className="border-b border-ring pb-1 pr-4">
          Training Specifications
        </span>
      </div>

      <div className="text-sm">{ai.profile?.trainingDescription}</div>
      {ai.profile?.showCharacter && (
        <div>
          <div className="text-xl font-bold mb-2">AI Model</div>
          <div className="text-sm">
            {aiModelRepository.findById(ai.modelId)?.name}
          </div>
          <div className="text-xl font-bold my-2">Instructions</div>
          <div className="text-sm">{ai.instructions}</div>
          <div className="text-xl font-bold my-2">Visibility</div>
          <div className="text-sm">{ai.visibility}</div>
        </div>
      )}

      {ai.profile?.showPersonality && (
        <div>
          <div className="text-xl font-bold my-2">Personality</div>
          <div className="text-sm">
            <div>Temperature: {(ai.options as any).temperature[0]}</div>
            <div>TopP: {(ai.options as any).topP[0]}</div>
            <div>Max Tokens: {(ai.options as any).maxTokens[0]}</div>
            <div>
              Frequency Penalty: {(ai.options as any).frequencyPenalty[0]}
            </div>
            <div>
              Presence Penalty: {(ai.options as any).presencePenalty[0]}
            </div>
          </div>
        </div>
      )}

      {ai.profile?.showTraining && !!dataSources.length && (
        <div>
          <div className="text-xl font-bold my-2">Training Knowledge</div>
          <ul className="list-disc pl-4">
            {dataSources.map((dataSource: any, index: number) => (
              <li key={`knowledge-${index}`}>
                <div className="text-sm truncate">{dataSource.name}</div>
              </li>
            ))}
          </ul>
        </div>
      )}

      <div className="text-3xl font-bold">
        <span className="border-b border-ring pb-1 pr-4">User Ratings</span>
      </div>
      <div>
        <StarRating
          value={Math.round(rating.averageRating)}
          count={rating.ratingCount}
          className=""
        />
        <div>{rating.ratingCount} User Ratings</div>
        <div>{rating.averageRating.toFixed(1)} out of 5</div>
        <Button variant="ring">
          <StarSvg className="h-4 w-4 mr-2 mb-1" fill="white" />
          Write a Review
        </Button>
      </div>
      <div className="grid rating-histogram-grid gap-3">
        {[...Array(5)].map((_, i) => (
          <>
            <div key={`rating-1-${i}`} className="text-ring">
              {i + 1} star
            </div>
            <div
              key={`rating-2-${i}`}
              className="border border-ring/30 rounded-md grow"
            ></div>
            <div key={`rating-3-${i}`} className="text-ring">
              0 %
            </div>
          </>
        ))}
      </div>
      <div className="text-xl font-bold">
        <span className="border-b border-ring pb-1 pr-8">Recent Reviews</span>
      </div>
    </div>
  );
};
