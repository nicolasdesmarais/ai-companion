import { Sheet, SheetContent, SheetTrigger } from "@/components/ui/sheet";
import { ReactNode } from "react";

type Props = {
  trigger: ReactNode;
  children: ReactNode;
  className?: string;
  open?: boolean;
  setOpen?: (open: boolean) => void;
};

export const Drawer = ({
  trigger,
  children,
  className,
  setOpen,
  open,
}: Props) => {
  return (
    <Sheet onOpenChange={setOpen} open={open}>
      <SheetTrigger>{trigger}</SheetTrigger>
      <SheetContent side="right" className="p-0 bg-secondary pt-10">
        {children}
      </SheetContent>
    </Sheet>
  );
};
