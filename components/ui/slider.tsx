import { cn } from "@/src/lib/utils";
import * as SliderPrimitive from "@radix-ui/react-slider";
import * as React from "react";

const Slider = React.forwardRef<
  React.ElementRef<typeof SliderPrimitive.Root>,
  React.ComponentPropsWithoutRef<typeof SliderPrimitive.Root>
>(({ className, ...props }, ref) => {
  const value = props.value || props.defaultValue;

  return (
    <SliderPrimitive.Slider
      className={cn(
        "flex items-center relative w-full h-3 select-none touch-none",
        className
      )}
      ref={ref}
      {...props}
      onValueChange={(val) => {
        if (props.onChange) props.onChange(val as any);
      }}
    >
      <SliderPrimitive.Track className="relative bg-muted-foreground grow h-2 rounded-full">
        <SliderPrimitive.Range className="absolute bg-ring h-full rounded-full" />
      </SliderPrimitive.Track>
      {value?.map((_, i) => (
        <SliderPrimitive.SliderThumb
          className="block w-6 h-6 bg-white rounded-3xl shadow-lg shadow-black"
          key={i}
        />
      ))}
    </SliderPrimitive.Slider>
  );
});
Slider.displayName = "FormMessage";

export { Slider };
