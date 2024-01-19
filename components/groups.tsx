"use client";

import { useGroupModal } from "@/hooks/use-group-modal";
import { GroupSummaryDto } from "@/src/domain/models/Groups";
import { cn } from "@/src/lib/utils";
import { useOrganization, useUser } from "@clerk/nextjs";
import { GroupAvailability } from "@prisma/client";
import axios from "axios";
import { MoreVertical, Plus } from "lucide-react";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";
import { useEffect, useState } from "react";

interface Props {
  groups: GroupSummaryDto[];
  hasElevatedWriteAccess: boolean;
}

export const Groups = ({ groups, hasElevatedWriteAccess }: Props) => {
  const router = useRouter();
  const searchParams = useSearchParams();
  const groupModal = useGroupModal();
  const { user } = useUser();
  const { organization } = useOrganization();
  const [groupList, setGroupList] = useState<GroupSummaryDto[]>(groups);

  const groupId = searchParams.get("groupId");
  const scope = searchParams.get("scope");

  const fetchGroups = async () => {
    const response = await axios.get("/api/v1/me/groups");
    if (response.status === 200 && Array.isArray(response.data.data)) {
      setGroupList(response.data.data);
    }
  };

  useEffect(() => {
    if (groupModal.areGroupsUpdated) {
      fetchGroups();
    }
  }, [groupModal.areGroupsUpdated]);

  const onClick = (id: string | undefined) => {
    let query;
    if (!id) {
      query = { scope, groupId: undefined };
    } else {
      query = { scope, groupId: id };
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

  if (
    scope !== "ORGANIZATION" &&
    scope !== "INSTANCE_ORGANIZATION" &&
    scope !== "ADMIN_ORGANIZATION" &&
    !groupId
  ) {
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
      {groupList.map((item) =>
        (scope && scope === "INSTANCE_ORGANIZATION") ||
        (scope && scope === "ADMIN_ORGANIZATION") ||
        (scope === "ORGANIZATION" && !item.notVisibleToMe) ? (
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
            (hasElevatedWriteAccess ||
              item.availability === GroupAvailability.RESTRICTED ||
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
        ) : null
      )}
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
