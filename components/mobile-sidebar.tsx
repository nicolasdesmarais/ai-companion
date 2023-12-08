import { Menu } from "lucide-react";

import { Sidebar } from "@/components/sidebar";
import { Sheet, SheetContent, SheetTrigger } from "@/components/ui/sheet";
import { Permission } from "@/src/security/models/Permission";

export const MobileSidebar = ({
  isPro,
  hasChat,
  userPermissions,
}: {
  isPro: boolean;
  hasChat: boolean;
  userPermissions: Permission[];
}) => {
  return (
    <Sheet>
      <SheetTrigger className="md:hidden pr-4" aria-controls="mobile-sidebar">
        <Menu />
      </SheetTrigger>
      <SheetContent
        side="left"
        className="p-0 bg-secondary pt-10 w-32"
        id="mobile-sidebar"
      >
        <Sidebar
          isPro={isPro}
          hasChat={hasChat}
          userPermissions={userPermissions}
        />
      </SheetContent>
    </Sheet>
  );
};
