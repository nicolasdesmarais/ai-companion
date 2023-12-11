import { GroupEntity } from "../../domain/models/GroupEntity";

export class GroupSecurityService {
  public static getGroupPermissions(
    orgId: string,
    userId: string,
    group: GroupEntity
  ) {
    return {
      canUpdateGroup: this.canUpdateGroup(orgId, userId, group),
      canInviteUsersToGroup: this.canInviteUsersToGroup(orgId, userId, group),
      canRemoveUsersFromGroup: this.canRemoveUsersFromGroup(
        orgId,
        userId,
        group
      ),
    };
  }

  public static canUpdateGroup(
    orgId: string,
    userId: string,
    group: GroupEntity
  ) {
    return group.ownerUserId === userId;
  }

  public static canInviteUsersToGroup(
    orgId: string,
    userId: string,
    group: GroupEntity
  ) {
    return true;
  }
  public static canRemoveUsersFromGroup(
    orgId: string,
    userId: string,
    group: GroupEntity
  ) {
    return true;
  }
}
