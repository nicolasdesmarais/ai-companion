"use client";
import { MobileSidebar } from "@/components/mobile-sidebar";
import { Permission } from "@/src/security/models/Permission";
import { Poppins } from "next/font/google";

const font = Poppins({ weight: "600", subsets: ["latin"] });
interface NavbarProps {
  isPro: boolean;
  userPermissions: Permission[];
}

export const Navbar = ({ isPro, userPermissions }: NavbarProps) => {
  return (
    <div className="fixed w-full z-30 flex justify-between items-center py-2 px-4 h-16 border-primary/10 bg-secondary md:hidden">
      <div className="flex items-center">
        <MobileSidebar isPro={isPro} userPermissions={userPermissions} />
      </div>
    </div>
  );
};
