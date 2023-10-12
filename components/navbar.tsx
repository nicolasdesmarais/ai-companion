"use client";
import { Sparkles } from "lucide-react";
import { Poppins } from "next/font/google";
import { MobileSidebar } from "@/components/mobile-sidebar";
import { Button } from "@/components/ui/button";
import { useProModal } from "@/hooks/use-pro-modal";

const font = Poppins({ weight: "600", subsets: ["latin"] });
interface NavbarProps {
  isPro: boolean;
}

export const Navbar = ({ isPro }: NavbarProps) => {
  const proModal = useProModal();

  return (
    <div className="fixed w-full z-30 flex justify-between items-center py-2 px-4 h-16 border-primary/10 bg-secondary md:hidden">
      <div className="flex items-center">
        <MobileSidebar isPro={isPro} />
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
