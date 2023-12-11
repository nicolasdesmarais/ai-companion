import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { ChatDetailDto } from "../../domain/ports/api/ChatsApi";

export class ChatSecurityService {
  /**
   * Returns true if the user has permission to read the specified chat.
   * @param authorizationContext
   * @param chat
   * @returns
   */

  public static canReadChat(
    authorizationContext: AuthorizationContext,
    chat: ChatDetailDto
  ): boolean {
    const { orgId, userId, permissions } = authorizationContext;

    for (const permission of permissions) {
      if (
        permission.resourceType === SecuredResourceType.CHATS &&
        permission.action === SecuredAction.READ
      ) {
        switch (permission.accessLevel) {
          case SecuredResourceAccessLevel.INSTANCE:
            return true;
          case SecuredResourceAccessLevel.ORGANIZATION:
            if (chat.orgId === orgId) {
              return true;
            }
          case SecuredResourceAccessLevel.SELF:
            if (chat.userId === userId) {
              return true;
            }
            break;
        }
      }
    }

    return false;
  }
}
