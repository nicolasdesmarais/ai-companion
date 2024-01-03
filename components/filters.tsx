"use client";

import {
  ListAIsRequestScope,
  SuperuserScopes,
} from "@/src/adapter-in/api/AIApi";
import { cn } from "@/src/lib/utils";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";

export const Filters = () => {
  const router = useRouter();
  const searchParams = useSearchParams();

  const scope = searchParams.get("scope");
  const groupId = searchParams.get("groupId");

  const onClick = (id: string | undefined) => {
    let query = { scope: id, groupId: undefined };

    const url = qs.stringifyUrl(
      {
        url: window.location.href,
        query,
      },
      { skipNull: true }
    );

    router.push(url);
  };

  const btnClassNames = `
    flex
    items-center
    text-center
    text-xs
    md:text-sm
    px-2
    md:px-4
    py-2
    md:py-3
    rounded-md
    bg-primary/10
    hover:opacity-75
    transition
  `;

  if (scope && SuperuserScopes.includes(scope as ListAIsRequestScope)) {
    return (
      <div className="w-full overflow-x-auto space-x-2 flex p-1">
        <button
          onClick={() => onClick("INSTANCE_ORGANIZATION")}
          className={cn(
            btnClassNames,
            scope === "INSTANCE_ORGANIZATION" || groupId
              ? "bg-accent"
              : "bg-primary/10"
          )}
        >
          Organization
        </button>
        <button
          onClick={() => onClick("INSTANCE_NOT_VISIBLE")}
          className={cn(
            btnClassNames,
            scope === "INSTANCE_NOT_VISIBLE" ? "bg-accent" : "bg-primary/10"
          )}
        >
          Not Visible to Me
        </button>
        <button
          onClick={() => onClick("INSTANCE_PRIVATE")}
          className={cn(
            btnClassNames,
            scope === "INSTANCE_PRIVATE" ? "bg-accent" : "bg-primary/10"
          )}
        >
          Private
        </button>
      </div>
    );
  }

  return (
    <div className="w-full overflow-x-auto space-x-2 flex p-1">
      {/* <button
        onClick={() => onClick("PUBLIC")}
        className={cn(
          btnClassNames,
          scope == "PUBLIC" ? "bg-accent" : "bg-primary/10"
        )}
      >
        Public
      </button> */}
      <button
        onClick={() => onClick("ORGANIZATION")}
        className={cn(
          btnClassNames,
          scope === "ORGANIZATION" || groupId ? "bg-accent" : "bg-primary/10"
        )}
      >
        Organization
      </button>
      <button
        onClick={() => onClick("PRIVATE")}
        className={cn(
          btnClassNames,
          scope === "PRIVATE" ? "bg-accent" : "bg-primary/10"
        )}
      >
        Private
      </button>
    </div>
  );
};
