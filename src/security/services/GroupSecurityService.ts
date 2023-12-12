import { GroupSummaryDto } from "@/src/domain/ports/api/GroupsApi";
import { AuthorizationContext } from "../models/AuthorizationContext";

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
}
