"use client";

import * as CheckboxPrimitive from "@radix-ui/react-checkbox";
import * as React from "react";
import { cn } from "@/src/lib/utils";
import { Check } from "lucide-react";

const Checkbox = React.forwardRef<
  React.ElementRef<typeof CheckboxPrimitive.Root>,
  React.ComponentPropsWithoutRef<typeof CheckboxPrimitive.Root>
>(({ className, children, id, ...props }, ref) => (
  <div className="flex my-2 items-center">
    <div className="h-5">
      <CheckboxPrimitive.Root
        id={id}
        className={cn(
          "h-5 w-5 bg-background rounded-md border border-input ring-offset-background focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 focus:bg-accent focus:text-accent-foreground data-[disabled]:pointer-events-none data-[disabled]:opacity-50",
          className
        )}
        {...props}
      >
        <CheckboxPrimitive.Indicator>
          <Check className="h-4 w-4" />
        </CheckboxPrimitive.Indicator>
      </CheckboxPrimitive.Root>
    </div>
    <div className="ml-2 h-6 align-middle">
      <label htmlFor={id}>{children}</label>
    </div>
  </div>
));

Checkbox.displayName = CheckboxPrimitive.Root.displayName;

export { Checkbox };
