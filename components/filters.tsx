"use client";

import { cn } from "@/src/lib/utils";
import { useOrganization, useUser } from "@clerk/nextjs";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";

export const Filters = () => {
  const router = useRouter();
  const searchParams = useSearchParams();

  const scope = searchParams.get("scope");
  const groupId = searchParams.get("groupId");

  const onClick = (id: string | undefined) => {
    let query;
    if (id === "PUBLIC") {
      query = { scope: "PUBLIC", groupId: undefined };
    } else if (id === "PRIVATE") {
      query = { scope: "PRIVATE", groupId: undefined };
    } else if (id === "ORGANIZATION") {
      query = { scope: "ORGANIZATION", groupId: undefined };
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

  return (
    <div className="w-full overflow-x-auto space-x-2 flex p-1">
      {/* <button
        onClick={() => onClick("PUBLIC")}
        className={cn(
          `
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
        `,
          scope == "PUBLIC" ? "bg-accent" : "bg-primary/10"
        )}
      >
        Public
      </button> */}
      <button
        onClick={() => onClick("ORGANIZATION")}
        className={cn(
          `
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
        `,
          scope === "ORGANIZATION" || groupId ? "bg-accent" : "bg-primary/10"
        )}
      >
        Organization
      </button>
      <button
        onClick={() => onClick("PRIVATE")}
        className={cn(
          `
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
        `,
          scope === "PRIVATE" ? "bg-accent" : "bg-primary/10"
        )}
      >
        Private
      </button>
    </div>
  );
};
