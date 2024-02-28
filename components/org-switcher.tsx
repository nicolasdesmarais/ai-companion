"use client";

import { useAuth, useClerk } from "@clerk/nextjs";
import { dark } from "@clerk/themes";
import Image from "next/image";

import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuGroup,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import useMatchMedia from "@/hooks/use-match-media";
import { ArrowLeftRight, Plus, Settings } from "lucide-react";
import { usePathname, useRouter } from "next/navigation";

const itemClass =
  "text-muted-foreground text-xs p-3 flex w-full font-medium cursor-pointer hover:text-primary hover:bg-primary/10 rounded-lg group";

export const OrgSwitcher = () => {
  const clerk = useClerk();
  const { has } = useAuth();
  const router = useRouter();
  const pathname = usePathname();
  const isMobile = useMatchMedia("(max-width: 768px)");

  let role = "User";
  if (has && has({ permission: "org:sys_profile:manage" })) {
    role = "Admin";
  }
  if (clerk.user?.publicMetadata && clerk.user?.publicMetadata.superuser) {
    role = "Superuser";
  }

  if (!clerk.loaded) return null;
  if (!clerk.organization?.id) return null;

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Image
          alt={clerk.organization?.name}
          src={clerk.organization?.imageUrl}
          width="64"
          height="64"
          className="rounded-lg cursor-pointer"
        />
      </DropdownMenuTrigger>
      <DropdownMenuContent
        align="end"
        side={isMobile ? "top" : "right"}
        sideOffset={isMobile ? 0 : 10}
        className="mt-2 w-full"
      >
        <DropdownMenuLabel>
          <div className="flex flex-row pl-4 pt-4 mb-1">
            <Image
              alt={clerk.organization?.name}
              src={clerk.organization?.imageUrl}
              width="44"
              height="44"
              className="rounded-lg"
            />
            <div className="pl-4">
              <div className="truncate max-w-64">
                {clerk.organization?.name}
              </div>
              <div className="text-muted-foreground text-xs mt-1">{role}</div>
            </div>
          </div>
        </DropdownMenuLabel>
        <DropdownMenuGroup>
          <div className={itemClass}>
            <DropdownMenuItem
              onClick={() =>
                clerk.openOrganizationProfile({
                  appearance: {
                    baseTheme: dark,
                  },
                })
              }
              className="cursor-pointer"
            >
              <Settings className="h-4 w-4 mr-6 ml-2" />
              Manage Organization
            </DropdownMenuItem>
          </div>
        </DropdownMenuGroup>
        <DropdownMenuSeparator />
        {clerk.user?.organizationMemberships?.map(
          (membership) =>
            membership.organization.id !== clerk.organization?.id && (
              <div className={itemClass} key={membership.id}>
                <DropdownMenuItem
                  onClick={() => {
                    clerk.setActive({
                      session: clerk.session?.id,
                      organization: membership.organization.id,
                    });
                    if (pathname.startsWith("/chat")) {
                      router.push("/");
                    }
                  }}
                  className="w-full cursor-pointer"
                >
                  <Image
                    alt={membership.organization.name}
                    src={membership.organization.imageUrl}
                    width="32"
                    height="32"
                    className="rounded-lg mr-4"
                  />
                  <div className="flex justify-between w-full">
                    <div className="truncate max-w-64">
                      {membership.organization.name}
                    </div>
                    <div>
                      <ArrowLeftRight className="h-5 w-5 ml-2 invisible group-hover:visible" />
                    </div>
                  </div>
                </DropdownMenuItem>
              </div>
            )
        )}
        <div className={itemClass}>
          <DropdownMenuItem
            onClick={() =>
              clerk.openCreateOrganization({
                appearance: {
                  baseTheme: dark,
                },
              })
            }
            className="cursor-pointer"
          >
            <div className="flex">
              <Plus className="h-5 w-5 mr-6 ml-2" />
              Create Organization
            </div>
          </DropdownMenuItem>
        </div>
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
