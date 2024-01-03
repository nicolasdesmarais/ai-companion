"use client";

import { useGroupModal } from "@/hooks/use-group-modal";
import { GroupSummaryDto } from "@/src/domain/models/Groups";
import { cn } from "@/src/lib/utils";
import { useOrganization, useUser } from "@clerk/nextjs";
import { GroupAvailability } from "@prisma/client";
import { MoreVertical, Plus } from "lucide-react";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";

interface Props {
  groups: GroupSummaryDto[];
}

export const Groups = ({ groups }: Props) => {
  const router = useRouter();
  const searchParams = useSearchParams();
  const groupModal = useGroupModal();
  const { user } = useUser();
  const { organization } = useOrganization();

  const groupId = searchParams.get("groupId");
  const scope = searchParams.get("scope");

  const onClick = (id: string | undefined) => {
    let query;
    if (!id) {
      query = { scope: "ORGANIZATION", groupId: undefined };
    } else {
      query = { scope: undefined, groupId: id };
    }

    const url = qs.stringifyUrl(
      {
        url: window.location.href,
        query,
      },
      { skipNull: true }
    );

    router.push(url);
  };

  const createGroup = () => {
    groupModal.onOpen();
  };

  if (scope !== "ORGANIZATION" && !groupId) {
    return null;
  }

  const buttonCn = `
    flex
    items-center
    text-center
    text-xs
    md:text-sm
    px-2
    md:px-3
    py-2
    md:py-3
    rounded-md
    bg-primary/10
    hover:opacity-75
    transition
  `;

  return (
    <div className="w-full overflow-x-auto space-x-2 flex p-1">
      <button
        onClick={() => onClick(undefined)}
        className={cn(buttonCn, !groupId ? "bg-accent" : "bg-primary/10")}
      >
        All
      </button>
      {groups.map((item) => (
        <button
          onClick={() => onClick(item.id)}
          className={cn(
            buttonCn,
            item.id === groupId ? "bg-accent" : "bg-primary/10"
          )}
          key={item.id}
        >
          {item.name}
          {item.id === groupId &&
          (item.availability === GroupAvailability.RESTRICTED ||
            (item.availability === GroupAvailability.EVERYONE &&
              item.ownerUserId === user?.id)) ? (
            <MoreVertical
              onClick={() => groupModal.onOpen(item.id)}
              className="w-6 h-6 ml-1 p-1 opacity-60 hover:opacity-100 hover:bg-primary/10 hover:rounded-full cursor-pointer"
            />
          ) : (
            ""
          )}
        </button>
      ))}
      {organization?.id && (
        <button
          onClick={() => createGroup()}
          className={cn(buttonCn, "bg-primary/10")}
        >
          <Plus className="w-6 h-6" />
        </button>
      )}
    </div>
  );
};
