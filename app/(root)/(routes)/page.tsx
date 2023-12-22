import { AIs } from "@/components/ais";
import { Categories } from "@/components/categories";
import { ConfirmModal } from "@/components/confirm-modal";
import { GroupModal } from "@/components/group-modal";
import { Groups } from "@/components/groups";
import { Filters } from "@/components/filters";
import { InviteButton } from "@/components/invite-button";
import { SearchInput } from "@/components/search-input";
import aiService from "@/src/domain/services/AIService";

import {
  ListAIsRequestParams,
  ListAIsRequestScope,
} from "@/src/adapter-in/api/AIApi";
import categoryService from "@/src/domain/services/CategoryService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";

interface RootPageProps {
  searchParams: {
    scope: string;
    groupId: string;
    categoryId: string;
    search: string;
    sort: string;
  };
}

const RootPage = async ({ searchParams }: RootPageProps) => {
  const authorizationContext = getUserAuthorizationContext();
  if (!authorizationContext) {
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
    sort: searchParams.sort,
  };

  const data = await aiService.findAIsForUser(
    authorizationContext,
    requestParams
  );

  const categories = await categoryService.getCategories();

  return (
    <div className="h-full px-4 space-y-2 pt-2">
      <div className="flex ">
        <h1 className="text-4xl font-bold whitespace-nowrap pt-2 pr-2">
          Browse AIs
        </h1>
        <Filters />
        <InviteButton />
      </div>
      <SearchInput />
      {!(scope !== ListAIsRequestScope.PUBLIC || searchParams.groupId) && (
        <Categories data={categories} />
      )}
      <Groups />
      <AIs data={data} authorizationContext={authorizationContext} />
      <GroupModal />
      <ConfirmModal />
    </div>
  );
};

export default RootPage;
