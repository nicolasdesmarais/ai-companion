"use client";

import { useAuth, useClerk } from "@clerk/nextjs";
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
import { ArrowLeftRight, Plus, Settings } from "lucide-react";

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
          <div className="flex flex-row">
            <Image
              alt={clerk.organization?.name}
              src={clerk.organization?.imageUrl}
              width="44"
              height="44"
              className="rounded-lg"
            />
            <div>
              <div>{clerk.organization?.name}</div>
              <div>{role}</div>
            </div>
          </div>
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
              <Settings className="h-5 w-5" />
              Manage Organization
            </button>
          </DropdownMenuItem>
        </DropdownMenuGroup>
        <DropdownMenuSeparator />
        {clerk.user?.organizationMemberships?.map(
          (membership) =>
            membership.organization.id !== clerk.organization?.id && (
              <DropdownMenuItem asChild key={membership.id}>
                <button onClick={() => {}}>
                  <Image
                    alt={membership.organization.name}
                    src={membership.organization.imageUrl}
                    width="32"
                    height="32"
                    className="rounded-lg"
                  />
                  {membership.organization.name}
                  <ArrowLeftRight className="h-5 w-5" />
                </button>
              </DropdownMenuItem>
            )
        )}
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
            <Plus className="h-5 w-5" />
            Create Organization
          </button>
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
