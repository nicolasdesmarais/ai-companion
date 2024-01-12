import { GroupSummaryDto } from "@/src/domain/models/Groups";
import { AuthorizationContext } from "../models/AuthorizationContext";
import { SecuredAction } from "../models/SecuredAction";
import { SecuredResourceAccessLevel } from "../models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "../models/SecuredResourceType";
import { BaseEntitySecurityService } from "./BaseEntitySecurityService";

export class GroupSecurityService {
  public static getGroupPermissions(
    authorizationContext: AuthorizationContext,
    group: GroupSummaryDto
  ) {
    return {
      canUpdateGroup: this.canUpdateGroup(authorizationContext, group),
      canInviteUsersToGroup: this.canInviteUsersToGroup(
        authorizationContext,
        group
      ),
      canRemoveUsersFromGroup: this.canRemoveUsersFromGroup(
        authorizationContext,
        group
      ),
    };
  }

  public static canUpdateGroup(
    authorizationContext: AuthorizationContext,
    group: GroupSummaryDto
  ) {
    const { userId } = authorizationContext;
    return group.ownerUserId === userId;
  }

  public static canInviteUsersToGroup(
    authorizationContext: AuthorizationContext,
    group: GroupSummaryDto
  ) {
    return true;
  }
  public static canRemoveUsersFromGroup(
    authorizationContext: AuthorizationContext,
    group: GroupSummaryDto
  ) {
    return true;
  }

  public static canAssignEveryoneAvailability(
    authorizationContext: AuthorizationContext
  ) {
    return BaseEntitySecurityService.hasPermission(
      authorizationContext,
      SecuredResourceType.GROUPS,
      SecuredAction.WRITE,
      SecuredResourceAccessLevel.ORGANIZATION
    );
  }
}
