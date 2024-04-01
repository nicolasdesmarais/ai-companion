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
                href="/landing2/"
                className="flex items-center md:mr-10 lg:mr-20"
              >
                <AppdirectSvg className="h-5 w-5" />
                AppDirect <span className="font-extrabold ml-2">AI</span>
              </Link>
            </li>
            <li>
              <Link href="/landing2/features">How it works</Link>
            </li>
            <li>
              <Link href="/landing2/solutions">Solutions</Link>
            </li>
            <li>
              <Link href="/landing2/pricing">Pricing</Link>
            </li>
            <li>
              <Link href="/landing2/enterprise">Enterprise</Link>
            </li>
            <li>
              <Link href="/landing2/resources">Resources</Link>
            </li>
          </ul>
        </div>
      </SheetContent>
    </Sheet>
  );
};
