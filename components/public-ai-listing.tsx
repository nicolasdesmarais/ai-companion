import { AIs } from "@/components/ais";
import { Categories } from "@/components/categories";
import { Groups } from "@/components/groups";
import { SearchInput } from "@/components/search-input";
import aiService from "@/src/domain/services/AIService";

import {
  ListAIsRequestParams,
  ListAIsRequestScope,
} from "@/src/adapter-in/api/AIApi";
import categoryService from "@/src/domain/services/CategoryService";
import { cn } from "@/src/lib/utils";
import { AuthorizationContextType } from "@/src/security/models/AuthorizationContext";
import { BadgeCheck, LogIn, UserPlus2 } from "lucide-react";
import Link from "next/link";
import { PublicSidebar } from "./public-sidebar";
import { Button } from "./ui/button";

interface Props {
  searchParams: {
    scope: string;
    groupId: string;
    categoryId: string;
    approvedByOrg: string;
    search: string;
    sort: string;
  };
  scopeParam?: string;
}

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
    bg-primary/10
    hover:opacity-75
    transition
    block
  `;

export const PublicAiListing = async ({ searchParams, scopeParam }: Props) => {
  let scope: ListAIsRequestScope | undefined;
  if (!scopeParam) {
    scope = undefined;
  } else {
    const scopeKey = scopeParam.toUpperCase().replace(/-/g, "_");
    if (
      !Object.values(ListAIsRequestScope).includes(
        scopeKey as ListAIsRequestScope
      )
    ) {
      scope = undefined;
    } else {
      scope = ListAIsRequestScope[scopeKey as keyof typeof ListAIsRequestScope];
    }
  }

  let approvedByOrg;
  if (searchParams.approvedByOrg === "true") {
    approvedByOrg = true;
  } else if (searchParams.approvedByOrg === "false") {
    approvedByOrg = false;
  }

  const requestParams: ListAIsRequestParams = {
    scope: ListAIsRequestScope.PUBLIC,
    groupId: searchParams.groupId,
    categoryId: searchParams.categoryId,
    search: searchParams.search,
    approvedByOrg,
    sort: searchParams.sort,
  };

  const data = await aiService.findPublicAIs(requestParams);

  const categories = await categoryService.getCategories();

  const hasElevatedWriteAccess = false;
  const groups = [] as any[];
  const authorizationContext = {
    orgId: "",
    userId: "",
    type: AuthorizationContextType.USER,
    permissions: [],
  };

  return (
    <div className="h-full">
      <div className="hidden md:flex h-full w-20 flex-col fixed inset-y-0 z-40">
        <PublicSidebar ais={data} />
      </div>
      <main className="md:pl-20 md:pt-0 h-full">
        <div className="h-full pr-4 pl-2 space-y-2 pt-2">
          <div className="flex justify-between">
            <div className="flex flex-col md:flex-row">
              <h1 className="text-4xl font-bold whitespace-nowrap pt-2 pr-2">
                Browse AIs
              </h1>
            </div>
            <div className="w-full overflow-x-auto space-x-0.5 p-1 hidden md:flex">
              <div className="flex space-x-0.5">
                <Link href="/sign-up" className="flex">
                  <button className={cn(btnClassNames, "rounded-l-md")}>
                    Public
                  </button>
                </Link>
                <Link href="/sign-up" className="flex">
                  <button className={cn(btnClassNames)}>Organization</button>
                </Link>
                <Link href="/sign-up" className="flex">
                  <button className={cn(btnClassNames, "rounded-r-md")}>
                    Private
                  </button>
                </Link>
              </div>
              <Link href="/sign-up">
                <button className={cn(btnClassNames, "flex rounded-md py-2")}>
                  <BadgeCheck className="w-6 h-6 mr-2 text-ring" />
                  Company Approved
                </button>
              </Link>
            </div>
            <div className="flex">
              <Link href="/sign-up" className="flex">
                <Button size="sm" variant="ring" className="my-2" type="button">
                  Invite
                  <UserPlus2 className="h-4 w-4 fill-white text-white ml-2 hidden md:inline" />
                </Button>
              </Link>
              <Link href="/sign-in" className="ml-2 flex">
                <Button
                  size="sm"
                  variant="ring"
                  className="my-2 text-nowrap"
                  type="button"
                >
                  Sign in
                  <LogIn className="h-4 w-4 text-white ml-2 hidden md:inline" />
                </Button>
              </Link>
            </div>
          </div>
          <SearchInput />
          <Categories data={categories} />
          <Groups
            groups={groups}
            hasElevatedWriteAccess={hasElevatedWriteAccess}
            scopeParam={scopeParam}
          />
          <AIs
            data={data}
            authorizationContext={authorizationContext}
            groups={groups}
            path="/public"
          />
        </div>
      </main>
    </div>
  );
};
