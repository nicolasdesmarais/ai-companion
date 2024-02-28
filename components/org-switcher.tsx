"use client";

import { useAuth, useClerk } from "@clerk/nextjs";
// import * as DropdownMenu from "@radix-ui/react-dropdown-menu";
import { dark } from "@clerk/themes";
import Image from "next/image";
import { useRouter } from "next/navigation";

import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuGroup,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";

export const OrgSwitcher = () => {
  const clerk = useClerk();
  const router = useRouter();
  const { has } = useAuth();

  let role = "User";
  if (has && has({ permission: "org:sys_profile:manage" })) {
    role = "Admin";
  }
  if (clerk.user?.publicMetadata && clerk.user?.publicMetadata.superuser) {
    role = "Superuser";
  }

  if (!clerk.loaded) return null;
  if (!clerk.organization?.id) return null;

  console.log(clerk.user?.publicMetadata);
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <button className="w-16">
          <Image
            alt={clerk.organization?.name}
            src={clerk.organization?.imageUrl}
            width="64"
            height="64"
            className="rounded-lg"
          />
        </button>
      </DropdownMenuTrigger>
      <DropdownMenuContent
        align="end"
        side="right"
        sideOffset={20}
        alignOffset={-20}
      >
        <DropdownMenuLabel>
          <Image
            alt={clerk.organization?.name}
            src={clerk.organization?.imageUrl}
            width="44"
            height="44"
            className="rounded-lg"
          />
          {clerk.organization?.name}
          {role}
        </DropdownMenuLabel>
        <DropdownMenuGroup>
          <DropdownMenuItem asChild>
            <button
              onClick={() =>
                clerk.openOrganizationProfile({
                  appearance: {
                    baseTheme: dark,
                  },
                })
              }
            >
              Manage Organization
            </button>
          </DropdownMenuItem>
        </DropdownMenuGroup>
        <DropdownMenuSeparator />
        <DropdownMenuItem asChild>
          <button
            onClick={() =>
              clerk.openCreateOrganization({
                appearance: {
                  baseTheme: dark,
                },
              })
            }
          >
            Create Organization
          </button>
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
