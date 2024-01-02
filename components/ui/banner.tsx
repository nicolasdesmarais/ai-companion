import { Info } from "lucide-react";
import { ReactNode } from "react";
import { cn } from "@/src/lib/utils";

interface BannerProps {
  children: ReactNode;
  className?: string;
  variant?: "info" | "destructive";
}

export const Banner = ({
  children,
  className,
  variant = "info",
}: BannerProps) => {
  return (
    <div
      className={cn(
        "flex items-center p-2 rounded-lg",
        className,
        variant === "info" ? "bg-accent" : "bg-destructive"
      )}
    >
      <Info className="h-5 w-5" />
      <div className="text-sm text-left ml-2">{children}</div>
    </div>
  );
};
