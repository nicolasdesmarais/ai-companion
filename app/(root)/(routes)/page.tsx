import { AIs } from "@/components/ais";
import { Categories } from "@/components/categories";
import { ConfirmModal } from "@/components/confirm-modal";
import { GroupModal } from "@/components/group-modal";
import { Groups } from "@/components/groups";
import { Filters } from "@/components/filters";
import { InviteButton } from "@/components/invite-button";
import { SearchInput } from "@/components/search-input";
import aiService from "@/src/domain/services/AIService";
import { Banner } from "@/components/ui/banner";

import {
  ListAIsRequestParams,
  ListAIsRequestScope,
  SuperuserScopes,
} from "@/src/adapter-in/api/AIApi";
import categoryService from "@/src/domain/services/CategoryService";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";
import { GroupSummaryDto } from "@/src/domain/models/Groups";
import groupService from "@/src/domain/services/GroupService";
import { SecuredRole } from "@/src/security/models/SecuredRoles";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";

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

  const hasInstanceGroupsAccess = BaseEntitySecurityService.hasPermission(
    authorizationContext,
    SecuredResourceType.GROUPS,
    SecuredAction.READ,
    SecuredResourceAccessLevel.INSTANCE
  );

  let groups: GroupSummaryDto[] = await groupService.findGroupsByUser(
    authorizationContext
  );
  if (
    scope === ListAIsRequestScope.INSTANCE_ORGANIZATION &&
    hasInstanceGroupsAccess
  ) {
    const allGroups: GroupSummaryDto[] = await groupService.getInstanceGroups();
    allGroups.forEach((group) => {
      if (!groups.find((g) => g.id === group.id)) {
        group.notVisibleToMe = true;
      }
    });
    groups = allGroups;
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
      {scope && SuperuserScopes.includes(scope) && (
        <Banner className="my-2" variant="destructive">
          Warning: As a superuser, you are able to see and edit AIs where the
          creator did not give you permission to see. Use with caution.
        </Banner>
      )}
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
      <Groups groups={groups} />
      <AIs
        data={data}
        authorizationContext={authorizationContext}
        groups={groups}
      />
      <GroupModal />
      <ConfirmModal />
    </div>
  );
};

export default RootPage;
