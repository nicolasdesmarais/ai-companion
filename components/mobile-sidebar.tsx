import { Menu } from "lucide-react";

import { ChatList } from "@/components/chat-list";
import { Sidebar } from "@/components/sidebar";
import { Sheet, SheetContent, SheetTrigger } from "@/components/ui/sheet";
import { cn } from "@/src/lib/utils";
import { Permission } from "@/src/security/models/Permission";
import { usePathname } from "next/navigation";
import { useState } from "react";

export const MobileSidebar = ({
  isPro,
  userPermissions,
  orgId,
}: {
  isPro: boolean;
  userPermissions: Permission[];
  orgId: string;
}) => {
  const [open, setOpen] = useState(false);
  const pathname = usePathname();
  return (
    <Sheet open={open} onOpenChange={setOpen}>
      <SheetTrigger className="md:hidden pr-4" aria-controls="mobile-sidebar">
        <Menu />
      </SheetTrigger>
      <SheetContent
        side="left"
        className={cn(
          "p-0 bg-secondary pt-10 ",
          pathname.startsWith("/chat") ? "w-56" : "w-32"
        )}
        id="mobile-sidebar"
      >
        <div className="flex h-full">
          <div className="w-32">
            <Sidebar
              isPro={isPro}
              userPermissions={userPermissions}
              orgId={orgId}
              setOpen={setOpen}
            />
          </div>
          {pathname.startsWith("/chat") && (
            <div className="w-32 flex justify-center">
              <ChatList className="bg-transparent/10" />
            </div>
          )}
        </div>
      </SheetContent>
    </Sheet>
  );
};
