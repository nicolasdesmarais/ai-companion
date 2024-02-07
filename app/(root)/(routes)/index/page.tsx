import { AIs } from "@/components/ais";
import { Categories } from "@/components/categories";
import { ConfirmModal } from "@/components/confirm-modal";
import { Filters } from "@/components/filters";
import { GroupModal } from "@/components/group-modal";
import { Groups } from "@/components/groups";
import { InviteButton } from "@/components/invite-button";
import { SearchInput } from "@/components/search-input";
import { Banner } from "@/components/ui/banner";
import aiService from "@/src/domain/services/AIService";

import {
  AdminScopes,
  ListAIsRequestParams,
  ListAIsRequestScope,
  SuperuserScopes,
} from "@/src/adapter-in/api/AIApi";
import { GroupSummaryDto } from "@/src/domain/models/Groups";
import categoryService from "@/src/domain/services/CategoryService";
import groupService from "@/src/domain/services/GroupService";
import { GroupSecurityService } from "@/src/security/services/GroupSecurityService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";

interface Props {
  searchParams: {
    scope: string;
    groupId: string;
    categoryId: string;
    approvedByOrg: string;
    search: string;
    sort: string;
  };
}

const IndexPage = async ({ searchParams }: Props) => {
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

  const groups: GroupSummaryDto[] = await groupService.findGroupsByUser(
    authorizationContext
  );

  let approvedByOrg;
  if (searchParams.approvedByOrg === "true") {
    approvedByOrg = true;
  } else if (searchParams.approvedByOrg === "false") {
    approvedByOrg = false;
  }

  const requestParams: ListAIsRequestParams = {
    scope: scope,
    groupId: searchParams.groupId,
    categoryId: searchParams.categoryId,
    search: searchParams.search,
    approvedByOrg,
    sort: searchParams.sort,
  };

  const data = await aiService.findAIsForUser(
    authorizationContext,
    requestParams
  );

  const categories = await categoryService.getCategories();

  const hasElevatedWriteAccess =
    GroupSecurityService.hasElevatedWriteAccess(authorizationContext);

  return (
    <div className="h-full pr-4 pl-2 space-y-2 pt-2">
      {scope && SuperuserScopes.includes(scope) && (
        <Banner className="my-2" variant="destructive">
          Warning: As a superuser, you are able to see and edit AIs where the
          creator did not give you permission to see. Use with caution.
        </Banner>
      )}
      {scope && AdminScopes.includes(scope) && (
        <Banner className="my-2" variant="destructive">
          Warning: As a company admin, you are able to see and edit AIs where
          the creator did not give you permission to see. Use with caution.
        </Banner>
      )}
      <div className="flex justify-between">
        <div className="flex flex-col md:flex-row">
          <h1 className="text-4xl font-bold whitespace-nowrap pt-2 pr-2">
            Browse AIs
          </h1>
          <Filters />
        </div>
        <InviteButton />
      </div>
      <SearchInput />
      {!(scope !== ListAIsRequestScope.PUBLIC || searchParams.groupId) && (
        <Categories data={categories} />
      )}
      <Groups groups={groups} hasElevatedWriteAccess={hasElevatedWriteAccess} />
      <AIs
        data={data}
        authorizationContext={authorizationContext}
        groups={groups}
      />
      <GroupModal hasElevatedWriteAccess={hasElevatedWriteAccess} />
      <ConfirmModal />
    </div>
  );
};

export default IndexPage;
