"use client";

import { ModeToggle } from "@/components/mode-toggle";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { useChats } from "@/hooks/use-chats";
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
  Building,
  Building2,
  Eye,
  FileText,
  LockKeyhole,
  MessageSquare,
  Plus,
  Settings,
  Sparkles,
  Store,
  UserPlus,
} from "lucide-react";
import {
  ReadonlyURLSearchParams,
  usePathname,
  useRouter,
  useSearchParams,
} from "next/navigation";
import { useEffect } from "react";
import { ProModal } from "./pro-modal";
import { Button } from "./ui/button";

interface SidebarProps {
  isPro: boolean;
  className?: string;
  userPermissions: Permission[];
  orgId: string;
}

interface Route {
  icon: any;
  href: string;
  pathname?: string;
  searchparams?: Record<string, string>;
  searchparamsregex?: Record<string, RegExp>;
  label: string;
  pro: boolean;
  regex: RegExp;
  requiredPermission?: Permission;
  children?: Route[];
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

  if (route.searchparamsregex) {
    const params = Object.fromEntries(searchparams.entries());
    const requiredParamsRegex = Object.entries(route.searchparamsregex);
    if (requiredParamsRegex.length) {
      const searchActive = requiredParamsRegex.every(([key, value]) =>
        value.test(params[key])
      );
      return pathActive && searchActive;
    }
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
  className,
  userPermissions,
  orgId,
}: SidebarProps) => {
  const { chats, fetchChats, loading } = useChats();
  const proModal = useProModal();
  const router = useRouter();
  const pathname = usePathname();
  const searchparams = useSearchParams();

  const showUpgrade = userPermissions.some((permission) => {
    return (
      permission.resourceType === SecuredResourceType.AI &&
      permission.action === SecuredAction.READ &&
      permission.accessLevel === SecuredResourceAccessLevel.ORGANIZATION
    );
  });
  useEffect(() => {
    fetchChats();
  }, [fetchChats]);

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
      href: "/index?scope=OWNED",
      pathname: "/index",
      searchparams: { scope: "OWNED" },
      label: "Your AIs",
      pro: false,
    },
    {
      icon: UserPlus,
      href: "/index?scope=SHARED",
      pathname: "/index",
      searchparams: { scope: "SHARED" },
      label: "Shared",
      pro: false,
    },
    {
      icon: FileText,
      href: "/data-sources",
      pathname: "/data-sources",
      label: "Data",
      pro: false,
    },
    {
      icon: Eye,
      href: "/index?scope=INSTANCE",
      pathname: "/index",
      searchparamsregex: { scope: /INSTANCE.*/ },
      label: "Super User",
      pro: false,
      requiredPermission: {
        resourceType: SecuredResourceType.AI,
        action: SecuredAction.READ,
        accessLevel: SecuredResourceAccessLevel.INSTANCE,
      },
    },
    {
      icon: Building2,
      href: "/index?scope=ADMIN",
      pathname: "/index",
      searchparamsregex: { scope: /ADMIN.*/ },
      label: "Admin",
      pro: false,
      requiredPermission: {
        resourceType: SecuredResourceType.AI,
        action: SecuredAction.READ,
        accessLevel: SecuredResourceAccessLevel.ORGANIZATION,
      },
    },
    {
      icon: Settings,
      label: "Settings",
      children: [
        {
          icon: Building,
          href: "/organization-settings",
          label: "Company Settings",
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
      ],
    },
  ] as Route[];
  const itemClass =
    "text-muted-foreground text-xs group py-3 px-8 flex w-full justify-center font-medium cursor-pointer hover:text-primary hover:bg-primary/10 rounded-lg transition";
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
              : loading || chats.length > 0
              ? "cursor-pointer hover:text-primary hover:bg-primary/10"
              : "opacity-25"
          )}
        >
          <div className="flex flex-col items-center flex-1">
            <MessageSquare className="h-5 w-5 mb-1" />
            Chat
          </div>
        </div>
        {routes.map((route) =>
          route.children ? (
            <div className={itemClass} key={route.label}>
              <DropdownMenu>
                <DropdownMenuTrigger asChild>
                  <div className="flex flex-col items-center flex-1">
                    <route.icon className="h-5 w-5 mb-1" />
                    <span className="w-12 text-center">{route.label}</span>
                  </div>
                </DropdownMenuTrigger>
                <DropdownMenuContent
                  align="end"
                  side="right"
                  className="min-w-[6rem] "
                  sideOffset={20}
                  alignOffset={-20}
                >
                  {route.children.map((child) => (
                    <DropdownMenuItem
                      key={child.href}
                      onClick={() => onNavigate(child.href, child.pro)}
                      className="focus:bg-transparent focus:text-primary focus:outline-none"
                    >
                      <div
                        className={cn(
                          itemClass,
                          "px-0",
                          isActive(child, pathname, searchparams) &&
                            "bg-accent text-primary",
                          shouldHideRoute(child) && "hidden"
                        )}
                      >
                        <div className="flex flex-col items-center flex-1">
                          <child.icon className="h-5 w-5 mb-1" />
                          <span className="w-12 text-center">
                            {child.label}
                          </span>
                        </div>
                      </div>
                    </DropdownMenuItem>
                  ))}
                </DropdownMenuContent>
              </DropdownMenu>
            </div>
          ) : (
            <div
              onClick={() => onNavigate(route.href, route.pro)}
              key={route.href}
              className={cn(
                itemClass,
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
          )
        )}
      </div>
      <div className="space-y-2 flex flex-col items-center py-3 px-8">
        {showUpgrade && (
          <Button onClick={proModal.onOpen} size="sm" variant="premium">
            <Sparkles className="h-4 w-4 fill-white text-white" />
          </Button>
        )}
        <ModeToggle />
        <UserButton
          afterSignOutUrl="/"
          appearance={{
            baseTheme: dark,
          }}
        />
      </div>
      <ProModal orgId={orgId} />
    </div>
  );
};
