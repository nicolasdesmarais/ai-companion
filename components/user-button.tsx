"use client";

import { useClerk } from "@clerk/nextjs";
import * as DropdownMenu from "@radix-ui/react-dropdown-menu";
import Image from "next/image";
import { useRouter } from "next/navigation";

export const OrgSwitcher = () => {
  const clerk = useClerk();
  const router = useRouter();

  if (!clerk.loaded) return null;
  if (!clerk.user?.id) return null;

  console.log(clerk.organization);
  return (
    <DropdownMenu.Root>
      <DropdownMenu.Trigger asChild>
        <button>
          <Image
            alt={clerk.user?.primaryEmailAddress?.emailAddress!}
            src={clerk.user?.imageUrl}
            width={30}
            height={30}
          />
          {clerk.user?.username
            ? clerk.user.username
            : clerk.user?.primaryEmailAddress?.emailAddress!}
        </button>
      </DropdownMenu.Trigger>
      <DropdownMenu.Portal>
        <DropdownMenu.Content>
          <DropdownMenu.Label />
          <DropdownMenu.Group>
            <DropdownMenu.Item asChild>
              <button onClick={() => clerk.openUserProfile()}>Profile</button>
            </DropdownMenu.Item>
            <DropdownMenu.Item></DropdownMenu.Item>
          </DropdownMenu.Group>
          <DropdownMenu.Separator />
          <DropdownMenu.Item asChild>
            <button onClick={() => clerk.signOut(() => router.push("/"))}>
              Sign Out{" "}
            </button>
          </DropdownMenu.Item>
        </DropdownMenu.Content>
      </DropdownMenu.Portal>
    </DropdownMenu.Root>
  );
};
