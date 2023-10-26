import { Categories } from "@/components/categories";
import { Companions } from "@/components/companions";
import { GroupModal } from "@/components/group-modal";
import { Groups } from "@/components/groups";
import { InviteButton } from "@/components/invite-button";
import { SearchInput } from "@/components/search-input";
import aiService from "@/src/domain/services/AIService";
import groupService from "@/src/domain/services/GroupService";

import {
  ListAIsRequestParams,
  ListAIsRequestScope,
} from "@/src/domain/services/dtos/ListAIsRequestParams";
import prismadb from "@/src/lib/prismadb";
import { auth } from "@clerk/nextjs";

interface RootPageProps {
  searchParams: {
    scope: string;
    groupId: string;
    categoryId: string;
    search: string;
  };
}

const RootPage = async ({ searchParams }: RootPageProps) => {
  const authorization = await auth();
  const orgId = authorization.orgId;
  const userId = authorization.userId;

  if (!userId) {
    return;
  }

  const scopeParam = searchParams.scope;
  let scope: ListAIsRequestScope | undefined;
  if (
    !scopeParam ||
    !Object.values(ListAIsRequestScope).includes(
      scopeParam as ListAIsRequestScope
    )
  ) {
    scope = undefined;
  } else {
    scope = ListAIsRequestScope[scopeParam as keyof typeof ListAIsRequestScope];
  }

  const requestParams: ListAIsRequestParams = {
    scope: scope,
    groupId: searchParams.groupId,
    categoryId: searchParams.categoryId,
    search: searchParams.search,
  };

  const data = await aiService.findAIsForUser(authorization, requestParams);

  const groups = await groupService.findGroupsByUser(orgId, userId);

  const categories = await prismadb.category.findMany();

  return (
    <div className="h-full px-4 space-y-2 pt-2">
      <div className="flex ">
        <h1 className="text-4xl font-bold whitespace-nowrap pt-2 pr-2">
          Browse AIs
        </h1>
        <Groups data={groups} />
        <InviteButton />
      </div>
      <SearchInput />
      {!(scope === ListAIsRequestScope.PRIVATE || searchParams.groupId) && (
        <Categories data={categories} />
      )}
      <Companions data={data} />
      <GroupModal />
    </div>
  );
};

export default RootPage;
