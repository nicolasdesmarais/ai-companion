"use client";

import { ModeToggle } from "@/components/mode-toggle";
import { useProModal } from "@/hooks/use-pro-modal";
import { cn } from "@/src/lib/utils";
import { Permission } from "@/src/security/models/Permission";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { OrganizationSwitcher, UserButton } from "@clerk/nextjs";
import { dark } from "@clerk/themes";
import {
  Atom,
  Eye,
  LockKeyhole,
  MessageSquare,
  Plus,
  Settings,
  Store,
  UserPlus,
} from "lucide-react";
import {
  ReadonlyURLSearchParams,
  usePathname,
  useRouter,
  useSearchParams,
} from "next/navigation";

interface SidebarProps {
  isPro: boolean;
  hasChat: boolean;
  className?: string;
  userPermissions: Permission[];
}

interface Route {
  icon: any;
  href: string;
  pathname?: string;
  searchparams?: Record<string, string>;
  label: string;
  pro: boolean;
  regex: RegExp;
  requiredPermission?: Permission;
}

const isActive = (
  route: Route,
  pathname: string,
  searchparams: ReadonlyURLSearchParams
) => {
  let pathActive;
  if (route.regex) {
    pathActive = route.regex.test(pathname);
  } else if (route.pathname) {
    pathActive = pathname === route.pathname;
  } else {
    pathname === route.href;
  }

  if (route.searchparams) {
    const params = Object.fromEntries(searchparams.entries());
    const requiredParams = Object.entries(route.searchparams);
    if (requiredParams.length === 0) {
      return pathActive;
    }
    const searchActive = requiredParams.every(([key, value]) =>
      value === null ? !params[key] : params[key] === value
    );
    return pathActive && searchActive;
  }
  return pathActive;
};

export const Sidebar = ({
  isPro,
  hasChat,
  className,
  userPermissions,
}: SidebarProps) => {
  const proModal = useProModal();
  const router = useRouter();
  const pathname = usePathname();
  const searchparams = useSearchParams();

  const shouldHideRoute = (route: Route) => {
    const requiredRoutePermission = route.requiredPermission;
    if (!requiredRoutePermission) {
      return false;
    }

    const hasPermission = userPermissions.some((permission) => {
      return (
        permission.resourceType === requiredRoutePermission.resourceType &&
        permission.action === requiredRoutePermission.action &&
        permission.accessLevel === requiredRoutePermission.accessLevel
      );
    });

    return !hasPermission;
  };

  const onNavigate = (url: string, pro: boolean) => {
    if (pro && !isPro) {
      return proModal.onOpen();
    }

    return router.push(url);
  };

  const routes = [
    {
      icon: Store,
      href: "/",
      pathname: "/",
      searchparams: { scope: null },
      label: "Browse",
      pro: false,
    },
    {
      icon: Plus,
      href: "/ai/new/edit",
      regex: /\/ai\/(.*)\/edit/,
      label: "Create",
      pro: false,
    },
    {
      icon: Atom,
      href: "/?scope=OWNED",
      pathname: "/",
      searchparams: { scope: "OWNED" },
      label: "Your AIs",
      pro: false,
    },
    {
      icon: UserPlus,
      href: "/?scope=SHARED",
      pathname: "/",
      searchparams: { scope: "SHARED" },
      label: "Shared",
      pro: false,
    },
    {
      icon: Eye,
      href: "/?scope=INSTANCE",
      pathname: "/",
      searchparams: { scope: "INSTANCE" },
      label: "Superuser",
      pro: false,
      requiredPermission: {
        resourceType: SecuredResourceType.AI,
        action: SecuredAction.READ,
        accessLevel: SecuredResourceAccessLevel.INSTANCE,
      },
    },
    {
      icon: Settings,
      href: "/organization-settings",
      label: "Settings",
      requiredPermission: {
        resourceType: SecuredResourceType.ORG_SETTINGS,
        action: SecuredAction.WRITE,
        accessLevel: SecuredResourceAccessLevel.INSTANCE,
      },
      pro: false,
    },
    {
      icon: LockKeyhole,
      href: "/api-keys",
      label: "API Keys",
      pro: false,
    },
  ] as Route[];
  return (
    <div
      className={cn(
        "p-3 flex-1 flex justify-between flex-col h-full",
        className
      )}
    >
      <div className="space-y-2 flex flex-col items-center">
        <div className="h-16">
          <OrganizationSwitcher
            hidePersonal={true}
            appearance={{
              baseTheme: dark,
            }}
          />
        </div>
        <div
          onClick={() => onNavigate(`/chat/`, false)}
          className={cn(
            "text-muted-foreground text-xs group py-3 px-8 flex w-full justify-center font-medium rounded-lg transition",
            pathname.startsWith("/chat/")
              ? "bg-accent text-primary cursor-pointer hover:text-primary hover:bg-primary/10"
              : hasChat
              ? "cursor-pointer hover:text-primary hover:bg-primary/10"
              : "opacity-25"
          )}
        >
          <div className="flex flex-col items-center flex-1">
            <MessageSquare className="h-5 w-5 mb-1" />
            Chat
          </div>
        </div>
        {routes.map((route) => (
          <div
            onClick={() => onNavigate(route.href, route.pro)}
            key={route.href}
            className={cn(
              "text-muted-foreground text-xs group py-3 px-8 flex w-full justify-center font-medium cursor-pointer hover:text-primary hover:bg-primary/10 rounded-lg transition",
              isActive(route, pathname, searchparams) &&
                "bg-accent text-primary",
              shouldHideRoute(route) && "hidden"
            )}
          >
            <div className="flex flex-col items-center flex-1">
              <route.icon className="h-5 w-5 mb-1" />
              <span className="w-12 text-center">{route.label}</span>
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
