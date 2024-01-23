"use client";
import { MobileSidebar } from "@/components/mobile-sidebar";
import { Button } from "@/components/ui/button";
import { useProModal } from "@/hooks/use-pro-modal";
import { Permission } from "@/src/security/models/Permission";
import { Sparkles } from "lucide-react";
import { Poppins } from "next/font/google";

const font = Poppins({ weight: "600", subsets: ["latin"] });
interface NavbarProps {
  isPro: boolean;
  userPermissions: Permission[];
}

export const Navbar = ({ isPro, userPermissions }: NavbarProps) => {
  const proModal = useProModal();
  return (
    <div className="fixed w-full z-30 flex justify-between items-center py-2 px-4 h-16 border-primary/10 bg-secondary md:hidden">
      <div className="flex items-center">
        <MobileSidebar isPro={isPro} userPermissions={userPermissions} />
      </div>
      <div className="flex items-center gap-x-3">
        {!isPro && (
          <Button
            onClick={proModal.onOpen}
            size="sm"
            variant="premium"
            className="hidden"
          >
            Upgrade
            <Sparkles className="h-4 w-4 fill-white text-white ml-2" />
          </Button>
        )}
      </div>
    </div>
  );
};
