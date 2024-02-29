"use client";

import { useClerk } from "@clerk/nextjs";
import { LogOut, X } from "lucide-react";
import Image from "next/image";
import { useRouter } from "next/navigation";

import { dark } from "@clerk/themes";

import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { Settings } from "lucide-react";

const itemClass =
  "text-muted-foreground text-xs p-3 flex w-full font-medium cursor-pointer hover:text-primary hover:bg-primary/10 rounded-lg group";

interface Props {
  setOpen?: (open: boolean) => void;
}

export const UserButton = ({ setOpen }: Props) => {
  const clerk = useClerk();
  const router = useRouter();
  const isMobile = setOpen !== undefined;

  if (!clerk.loaded) return null;
  if (!clerk.user?.id) return null;

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Image
          alt={clerk.user?.primaryEmailAddress?.emailAddress || "User Image"}
          src={clerk.user?.imageUrl}
          width={32}
          height={32}
          className="rounded-full cursor-pointer"
        />
      </DropdownMenuTrigger>
      <DropdownMenuContent
        align="start"
        side={isMobile ? "top" : "right"}
        sideOffset={isMobile ? 0 : 35}
        className="mb-2 w-[98vw] h-[99vh] relative top-[60px] left-[-4px] md:static md:w-max md:h-auto"
      >
        <div
          onClick={() => setOpen && setOpen(false)}
          className="md:hidden absolute right-4 top-4 rounded-sm opacity-70 ring-offset-background transition-opacity hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 cursor-pointer"
        >
          <X className="h-4 w-4" />
        </div>
        <DropdownMenuLabel>
          <div className="flex flex-row pl-4 pt-4 mb-1">
            <Image
              alt={clerk.user?.fullName || "User Image"}
              src={clerk.user?.imageUrl}
              width="44"
              height="44"
              className="rounded-lg"
            />
            <div className="pl-4">
              <div className="truncate max-w-64">{clerk.user?.fullName}</div>
              <div className="text-muted-foreground text-xs mt-1">
                {clerk.user?.primaryEmailAddress?.emailAddress}
              </div>
            </div>
          </div>
        </DropdownMenuLabel>
        <div className={itemClass}>
          <DropdownMenuItem
            onClick={() => {
              if (isMobile) {
                setOpen(false);
              }
              clerk.openUserProfile({
                appearance: {
                  baseTheme: dark,
                },
              });
            }}
            className="cursor-pointer"
          >
            <Settings className="h-4 w-4 mr-6 ml-2" />
            Manage account
          </DropdownMenuItem>
        </div>
        <div className={itemClass}>
          <DropdownMenuItem
            onClick={() => {
              if (isMobile) {
                setOpen(false);
              }
              clerk.signOut(() => router.push("/"));
            }}
            className="cursor-pointer"
          >
            <div className="flex">
              <LogOut className="h-4 w-4 mr-6 ml-2" />
              Sign Out
            </div>
          </DropdownMenuItem>
        </div>
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
