"use client";

import { Home, Plus, Settings, Wrench } from "lucide-react";
import { usePathname, useRouter } from "next/navigation";
import { OrganizationSwitcher, UserButton } from "@clerk/nextjs";
import { dark } from "@clerk/themes";
import { ModeToggle } from "@/components/mode-toggle";
import { cn } from "@/lib/utils";
import { useProModal } from "@/hooks/use-pro-modal";

interface SidebarProps {
  isPro: boolean;
}

export const Sidebar = ({ isPro }: SidebarProps) => {
  const proModal = useProModal();
  const router = useRouter();
  const pathname = usePathname();

  const onNavigate = (url: string, pro: boolean) => {
    if (pro && !isPro) {
      return proModal.onOpen();
    }

    return router.push(url);
  };

  const routes = [
    {
      icon: Home,
      href: "/",
      label: "Home",
      pro: false,
    },
    {
      icon: Plus,
      href: "/companion/new",
      label: "Create",
      pro: false,
    },
    {
      icon: Wrench,
      href: "/dashboard",
      label: "Tools",
      pro: true,
    },
    {
      icon: Settings,
      href: "/settings",
      label: "Settings",
      pro: true,
    },
  ];

  return (
    <div className="p-3 flex-1 flex justify-between flex-col h-full">
      <div className="space-y-2 flex flex-col items-center">
        <OrganizationSwitcher
          appearance={{
            baseTheme: dark,
          }}
        />
        {routes.map((route) => (
          <div
            onClick={() => onNavigate(route.href, route.pro)}
            key={route.href}
            className={cn(
              "text-muted-foreground text-xs group py-3 px-8 flex w-full justify-center font-medium cursor-pointer hover:text-primary hover:bg-primary/10 rounded-lg transition",
              pathname === route.href && "bg-accent text-primary",
              route.pro && "hidden"
            )}
          >
            <div className="flex flex-col items-center flex-1">
              <route.icon className="h-5 w-5 mb-1" />
              {route.label}
            </div>
          </div>
        ))}
      </div>
      <div className="space-y-2 flex flex-col items-center py-3 px-8">
        <ModeToggle />
        <UserButton
          afterSignOutUrl="/"
          appearance={{
            baseTheme: dark,
          }}
        />
      </div>
    </div>
  );
};
