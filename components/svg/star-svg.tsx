import { cn } from "@/src/lib/utils";

interface StarSvgProps {
  fill?: string;
  className?: string;
}

export const StarSvg = ({ fill, className }: StarSvgProps) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      role="img"
      aria-label="starIcon"
      viewBox="0 0 32 32"
      className={cn(className)}
    >
      <path
        d="M19.964 11.91h9.676c.994 0 1.406 1.27.604 1.854l-7.83 5.688 2.99 9.202c.308.944-.772 1.73-1.576 1.146L16 24.112 8.172 29.8c-.804.584-1.884-.202-1.576-1.146l2.99-9.202-7.828-5.688c-.804-.584-.392-1.854.602-1.854h9.676l2.99-9.202c.306-.944 1.642-.944 1.948 0l2.99 9.202z"
        fill={fill || "#474747"}
      ></path>
    </svg>
  );
};
