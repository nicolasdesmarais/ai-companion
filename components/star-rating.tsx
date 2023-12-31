"use client";
import { cn } from "@/src/lib/utils";
import { StarSvg } from "./svg/star-svg";
import { useState } from "react";

const abbreviateNumber = (value: number) => {
  let newValue = value;
  const suffixes = ["", "K", "M", "B", "T"];
  let suffixNum = 0;
  while (newValue >= 1000) {
    newValue /= 1000;
    suffixNum++;
  }

  return suffixNum === 0
    ? value
    : `${newValue.toFixed(1)}${suffixes[suffixNum]}`;
};

interface StarRatingProps {
  maxStars?: number;
  count?: number;
  className?: string;
  size?: string;
  value?: number;
  onChange?: (src: number) => void;
  hideCount?: boolean;
  onClick?: () => void;
}

const getSizeClass = (size: string) => {
  switch (size) {
    case "small":
      return "w-4 h-4";
    case "medium":
      return "w-6 h-6";
    case "large":
      return "w-12 h-12";
    default:
      return "w-4 h-4";
  }
};

export const StarRating = ({
  maxStars = 5,
  className,
  value,
  count,
  size = "small",
  onChange,
  hideCount,
  onClick,
}: StarRatingProps) => {
  const [hover, setHover] = useState<number | null>(null);
  return (
    <>
      <div className={cn(className, "flex items-center")} onClick={onClick}>
        {[...Array(maxStars)].map((_, i) => (
          <button
            type="button"
            key={i}
            onMouseEnter={() => !!onChange && setHover(i + 1)}
            onMouseLeave={() => !!onChange && setHover(null)}
            onClick={() => !!onChange && onChange(i + 1)}
          >
            <StarSvg
              className={cn(
                getSizeClass(size),
                count ? "text-primary" : "text-primary/20",
                !!onChange && "cursor-pointer"
              )}
              fill={i < (hover || value || 0) ? "#eecc50" : ""}
            />
          </button>
        ))}
        {!onChange && !hideCount && (
          <span
            className={cn(
              "text-xs ml-1",
              count ? "text-muted-foreground" : "text-primary/20"
            )}
          >
            ({!count ? "No Reviews" : abbreviateNumber(count)})
          </span>
        )}
      </div>
    </>
  );
};
