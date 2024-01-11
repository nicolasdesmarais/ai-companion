"use client";
import * as TooltipPrimitives from "@radix-ui/react-tooltip";
import { cn } from "@/src/lib/utils";
import { ReactNode } from "react";

type Props = {
  children: ReactNode;
  content: ReactNode;
  className?: string;
  side?: "top" | "bottom" | "left" | "right";
};
export const Tooltip = ({
  children,
  className,
  content,
  side = "top",
}: Props) => {
  return (
    <TooltipPrimitives.Provider>
      <TooltipPrimitives.Root>
        <TooltipPrimitives.Trigger className={className}>
          {children}
        </TooltipPrimitives.Trigger>
        <TooltipPrimitives.Portal>
          <TooltipPrimitives.Content
            side={side}
            className={cn("text-xs bg-card p-2 rounded-sm")}
          >
            {content}
            <TooltipPrimitives.Arrow />
          </TooltipPrimitives.Content>
        </TooltipPrimitives.Portal>
      </TooltipPrimitives.Root>
    </TooltipPrimitives.Provider>
  );
};
