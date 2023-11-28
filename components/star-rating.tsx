import { cn } from "@/src/lib/utils";
import { StarSvg } from "./svg/star-svg";

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
  rating?: number;
  count?: number;
  className?: string;
}

export const StarRating = ({
  maxStars = 5,
  className,
  rating,
  count,
}: StarRatingProps) => {
  return (
    <>
      <div className={cn(className, "flex items-center")}>
        {[...Array(maxStars)].map((_, i) => (
          <StarSvg
            key={i}
            className={cn(
              "w-4 h-4 ",
              count ? "text-primary" : "text-primary/20"
            )}
          />
        ))}
        <span
          className={cn(
            "text-xs ml-1",
            count ? "text-muted-foreground" : "text-primary/20"
          )}
        >
          ({!count ? "No Reviews" : abbreviateNumber(count)})
        </span>
      </div>
    </>
  );
};
