"use client";
import { InviteButton } from "@/components/invite-button";
import { MobileSidebar } from "@/components/mobile-sidebar";
import { Permission } from "@/src/security/models/Permission";
import { Poppins } from "next/font/google";
import { usePathname } from "next/navigation";

const font = Poppins({ weight: "600", subsets: ["latin"] });
interface NavbarProps {
  isPro: boolean;
  userPermissions: Permission[];
  orgId: string;
}

export const Navbar = ({ isPro, userPermissions, orgId }: NavbarProps) => {
  const pathname = usePathname();
  const isIndex =
    pathname === "/" ||
    pathname.startsWith("/index") ||
    pathname.startsWith("/public/index");
  const isEditor = pathname.startsWith("/ai");
  const isData = pathname.startsWith("/data-sources");
  let title = null;
  if (isIndex) {
    title = "Browse AIs";
  } else if (isData) {
    title = "Data Sources";
  } else if (isEditor) {
    title = "Edit AI";
  }

  return (
    <div className="fixed w-full z-30 flex justify-between items-center py-2 px-4 border-primary/10 bg-secondary md:hidden">
      <div className="flex">
        <MobileSidebar
          isPro={isPro}
          userPermissions={userPermissions}
          orgId={orgId}
        />
      </div>
      {title && (
        <div className="flex justify-between w-full">
          <div className="flex flex-col md:flex-row justify-center">
            <h1 className="text-3xl font-bold whitespace-nowrap pr-2">
              {title}
            </h1>
          </div>
          {isIndex && <InviteButton />}
        </div>
      )}
    </div>
  );
};
