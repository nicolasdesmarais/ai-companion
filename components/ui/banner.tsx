import { Info } from "lucide-react";
import { ReactNode } from "react";
import { cn } from "@/src/lib/utils";

interface BannerProps {
  children: ReactNode;
  className?: string;
}

export const Banner = ({ children, className }: BannerProps) => {
  return (
    <div
      className={cn("flex items-center bg-accent p-2 rounded-lg", className)}
    >
      <Info className="h-5 w-5" />
      <p className="text-sm text-left ml-2">{children}</p>
    </div>
  );
};
