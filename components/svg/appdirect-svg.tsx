import { cn } from "@/src/lib/utils";

interface Props {
  fill?: string;
  className?: string;
}

export const AppdirectSvg = ({ fill, className }: Props) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      role="img"
      aria-label="appdirectIcon"
      width="50"
      height="35"
      viewBox="0 0 502.1 467.35"
      focusable="false"
      className={cn(className)}
      fill={fill || "#011B58"}
    >
      <title>AppDirect</title>
      <polygon
        points="150.36 0 253.68 0 455.35 317.43 365.06 355.75 150.36 0"
        className="opacity-85"
      />
      <polygon
        points="502.1 187.67 455.35 317.43 102.08 467.35 0 398.96 502.1 187.67"
        className="opacity-75"
      />
      <polygon
        points="150.36 0 253.68 0 102.08 467.35 0 398.96 150.36 0"
        className="opacity-95"
      />
    </svg>
  );
};
