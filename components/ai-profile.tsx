"use client";
import { ResizePanel } from "@/components/resize-panel";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { useAIProfile } from "@/hooks/use-ai-profile";
import { useRateAI } from "@/hooks/use-rate-ai";
import { StaticAIModelRepository } from "@/src/adapter-out/repositories/StaticAIModelRepository";
import { AIDetailDto } from "@/src/domain/models/AI";
import { cn } from "@/src/lib/utils";
import axios from "axios";
import { X } from "lucide-react";
import Image from "next/image";
import { useParams, useRouter } from "next/navigation";
import { useEffect, useState } from "react";
import { StarRating } from "./star-rating";
import { StarSvg } from "./svg/star-svg";
import { Button } from "./ui/button";

const aiModelRepository = new StaticAIModelRepository();
interface Props {
  ai: AIDetailDto;
}

const getUserName = (user: any) => {
  if (user.firstName && user.lastName) {
    return `${user.firstName} ${user.lastName}`;
  }
  if (user.emailAddresses.length && user.emailAddresses[0].emailAddress) {
    return user.emailAddresses[0].emailAddress;
  }
  return "User";
};

export const AIProfile = ({ ai }: Props) => {
  const { isOpen, onClose, onOpen } = useAIProfile();
  const [dataSources, setDataSources] = useState<any[]>([]);
  const [ratings, setRatings] = useState<any[]>([]);
  const [ratingDistributions, setRatingDistributions] = useState<any[]>([]);
  const rateAI = useRateAI();
  const params = useParams();
  const router = useRouter();

  useEffect(() => {
    const hashes = ["#profile", "#specifications", "#user-ratings"];
    if (hashes.includes(window.location.hash)) {
      onOpen();
    }
  }, [params]);

  useEffect(() => {
    if (isOpen && window.location.hash) {
      const anchor = document.querySelector(window.location.hash);
      if (anchor) {
        anchor.scrollIntoView({ behavior: "smooth" });
      }
    }
  }, [isOpen, params]);

  useEffect(() => {
    const fetchDataSources = async () => {
      const response = await axios.get(`/api/v1/ai/${ai.id}/data-sources`);
      setDataSources(response.data.data);
    };
    const fetchRatings = async () => {
      const response = await axios.get(`/api/v1/ai/${ai.id}/rating/all`);
      setRatings(response.data.reviews);
      setRatingDistributions(response.data.distributions);
    };
    if (ai.profile?.showTraining) {
      fetchDataSources();
    }
    fetchRatings();
  }, []);

  const showTraining =
    ai.profile?.showCharacter ||
    ai.profile?.showTraining ||
    ai.profile?.showPersonality ||
    ai.profile?.trainingDescription;

  return (
    <ResizePanel
      name="profile-resize-panel"
      initial={575}
      min={415}
      max={2000}
      position="right"
      className={cn("flex", !isOpen && "hidden")}
      animationClassName="animate-in slide-in-from-right duration-200"
      persist={false}
    >
      <div className="bg-[#0f111e] px-6 space-y-4 w-full md:w-auto absolute left-0 md:left-[76px] right-0 top-[64px] md:top-0 bottom-0 lg:static overflow-auto pb-16">
        <div className="absolute top-4 right-4">
          <Button
            onClick={() => {
              router.push("#");
              onClose();
            }}
            variant="ghost"
            size="icon"
            type="button"
          >
            <X className="h-6 w-6" />
          </Button>
        </div>
        <div className="pt-1">
          <a href="#profile">
            <Button
              variant="link"
              size="sm"
              type="button"
              onClick={() => router.push("#profile")}
            >
              Profile
            </Button>
          </a>
          {showTraining && (
            <a href="#specifications">
              <Button
                variant="link"
                size="sm"
                type="button"
                onClick={() => router.push("#specifications")}
              >
                Specifications
              </Button>
            </a>
          )}
          <a href="#user-ratings">
            <Button
              variant="link"
              size="sm"
              type="button"
              onClick={() => router.push("#user-ratings")}
            >
              Reviews
            </Button>
          </a>
        </div>
        <div className="flex" id="profile">
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
              value={Math.round(ai.rating)}
              count={ai.ratingCount}
              className=""
            />
          </div>
        </div>
        <div className="text-2xl font-bold">{ai.profile?.headline}</div>
        <div className="text-sm">{ai.profile?.description}</div>
        {ai.profile?.features.length > 0 && (
          <div className="text-2xl font-bold">
            <span className="border-b border-ring pb-1 pr-4">Features</span>
          </div>
        )}
        {ai.profile?.features.map((feature: any, index: number) => (
          <div key={`feature-${index}`}>
            <div className="text-xl font-bold mb-2">{feature.title}</div>
            <div className="text-sm">{feature.description}</div>
          </div>
        ))}
        {showTraining && (
          <div className="text-2xl font-bold" id="specifications">
            <span className="border-b border-ring pb-1 pr-4">
              Training Specifications
            </span>
          </div>
        )}

        <div className="text-sm">{ai.profile?.trainingDescription}</div>
        {ai.profile?.showCharacter && (
          <div>
            <div className="text-xl font-bold mb-2">AI Model</div>
            <div className="text-sm">
              {ai.modelId ? aiModelRepository.findById(ai.modelId)?.name : ""}
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

        <div className="text-3xl font-bold" id="user-ratings">
          <span className="border-b border-ring pb-1 pr-4">User Ratings</span>
        </div>
        <div className="flex justify-between flex-wrap">
          <div>
            <div className="flex">
              <StarRating
                value={Math.round(ai.rating)}
                count={ai.ratingCount}
                size="medium"
                hideCount={true}
              />
              <div className="ml-2 text-md">
                {ai.rating && `${ai.rating.toFixed(1)} out of 5`}
              </div>
            </div>
            <div className="text-xs text-muted-foreground mt-2 mb-2">
              {ai.ratingCount} User Ratings
            </div>
          </div>
          <Button variant="ring" onClick={() => rateAI.onOpen()}>
            <StarSvg className="h-4 w-4 mr-2 mb-1" fill="white" />
            Write a Review
          </Button>
        </div>
        {ai.rating && (
          <>
            <div className="grid gap-3 grid-cols-[50px_auto_50px]">
              {[...Array(5)].map((_, i) => (
                <>
                  <div key={`rating-1-${i}`} className="text-ring">
                    {5 - i} star
                  </div>
                  <div
                    key={`rating-2-${i}`}
                    className="border border-ring/30 rounded-md"
                  >
                    <div
                      className={cn(
                        "rounded-md bg-yellow h-full",
                        ratingDistributions[4 - i] < 100 && "rounded-r-none"
                      )}
                      style={{ width: `${ratingDistributions[4 - i] || 0}%` }}
                    ></div>
                  </div>
                  <div key={`rating-3-${i}`} className="text-ring">
                    {ratingDistributions[4 - i]} %
                  </div>
                </>
              ))}
            </div>
            <div className="text-xl font-bold">
              <span className="border-b border-ring pb-1 pr-8">
                Recent Reviews
              </span>
            </div>
            {ratings.map((rating: any, index: number) => (
              <div key={`rating-${index}`} className="pb-6">
                <div className="flex items-center mb-2">
                  <Avatar className="h-10 w-10">
                    <AvatarImage src={rating.user.imageUrl} crop="w_48,h_48" />
                  </Avatar>
                  <div className="text-md ml-4">{getUserName(rating.user)}</div>
                </div>
                <div className="flex">
                  <StarRating value={rating.rating} hideCount={true} />
                  <div className="text-md font-bold ml-4">
                    {rating.headline || rating.review}
                  </div>
                </div>
                {rating.headline && (
                  <div className="text-sm mt-2">{rating.review}</div>
                )}
              </div>
            ))}
          </>
        )}
      </div>
    </ResizePanel>
  );
};
