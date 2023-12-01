import { Sheet, SheetContent, SheetTrigger } from "@/components/ui/sheet";
import { ReactNode } from "react";

type Props = {
  trigger: ReactNode;
  children: ReactNode;
  className?: string;
};

export const Drawer = ({ trigger, children, className }: Props) => {
  return (
    <Sheet>
      <SheetTrigger>{trigger}</SheetTrigger>
      <SheetContent side="right" className="p-0 bg-secondary pt-10">
        {children}
      </SheetContent>
    </Sheet>
  );
};
