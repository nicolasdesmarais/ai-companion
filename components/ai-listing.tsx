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
import { PaywallBanner } from "./paywall-banner";

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

export const AiListing = async ({ searchParams, scopeParam }: Props) => {
  const authorizationContext = getUserAuthorizationContext();
  if (!authorizationContext) {
    return;
  }

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
    <div className="h-full pr-4 pl-2 space-y-2 pt-2 mt-16 md:mt-0">
      <PaywallBanner />
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
          <h1 className="text-4xl font-bold whitespace-nowrap pt-2 pr-2 hidden md:block">
            Browse AIs
          </h1>
          <Filters />
        </div>
        <InviteButton className="hidden md:flex" />
      </div>
      <SearchInput />
      {!(scope !== ListAIsRequestScope.PUBLIC || searchParams.groupId) && (
        <Categories data={categories} />
      )}
      <Groups
        groups={groups}
        hasElevatedWriteAccess={hasElevatedWriteAccess}
        scopeParam={scopeParam}
      />
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
