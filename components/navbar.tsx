"use client";
import { MobileSidebar } from "@/components/mobile-sidebar";
import { Button } from "@/components/ui/button";
import { useProModal } from "@/hooks/use-pro-modal";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { Sparkles } from "lucide-react";
import { Poppins } from "next/font/google";

const font = Poppins({ weight: "600", subsets: ["latin"] });
interface NavbarProps {
  isPro: boolean;
  hasChat: boolean;
}

export const Navbar = ({ isPro, hasChat }: NavbarProps) => {
  const proModal = useProModal();

  const authorizationContext = getUserAuthorizationContext();
  if (!authorizationContext) {
    return;
  }

  const { userId, permissions } = authorizationContext;

  return (
    <div className="fixed w-full z-30 flex justify-between items-center py-2 px-4 h-16 border-primary/10 bg-secondary md:hidden">
      <div className="flex items-center">
        <MobileSidebar
          isPro={isPro}
          hasChat={hasChat}
          userPermissions={permissions}
        />
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
