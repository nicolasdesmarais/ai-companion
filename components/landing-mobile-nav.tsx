"use client";
import { Sheet, SheetContent, SheetTrigger } from "@/components/ui/sheet";
import { cn } from "@/src/lib/utils";
import { Menu } from "lucide-react";
import Link from "next/link";
import { useState } from "react";
import { AppdirectSvg } from "./svg/appdirect-svg";

export const MobileNav = () => {
  const [open, setOpen] = useState(false);
  return (
    <Sheet open={open} onOpenChange={setOpen}>
      <SheetTrigger className="md:hidden pr-4" aria-controls="mobile-sidebar">
        <Menu />
      </SheetTrigger>
      <SheetContent
        side="left"
        className={cn("p-0 bg-secondary pt-10 bg-white text-navy w-64")}
      >
        <div className="flex h-full">
          <ul className="p-8 gap-8 flex flex-col">
            <li>
              <Link
                href="/landing/"
                className="flex items-center md:mr-10 lg:mr-20"
                onClick={() => setOpen(false)}
              >
                <AppdirectSvg className="h-5 w-5" />
                AppDirect <span className="font-extrabold ml-2">AI</span>
              </Link>
            </li>
            <li>
              <Link href="/landing/features" onClick={() => setOpen(false)}>
                How it works
              </Link>
            </li>
            <li>
              <Link href="/landing/solutions" onClick={() => setOpen(false)}>
                Solutions
              </Link>
            </li>
            <li>
              <Link href="/landing/pricing" onClick={() => setOpen(false)}>
                Pricing
              </Link>
            </li>
            <li>
              <Link href="/landing/enterprise" onClick={() => setOpen(false)}>
                Enterprise
              </Link>
            </li>
            <li>
              <Link href="/landing/resources" onClick={() => setOpen(false)}>
                Resources
              </Link>
            </li>
          </ul>
        </div>
      </SheetContent>
    </Sheet>
  );
};
