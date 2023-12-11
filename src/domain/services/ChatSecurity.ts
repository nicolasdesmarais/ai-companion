import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { ChatDetailDto } from "../ports/api/ChatsApi";

export class ChatSecurityService {
  public static canReadChat(
    authorizationContext: AuthorizationContext,
    chat: ChatDetailDto
  ): boolean {
    const { userId, permissions } = authorizationContext;

    let hasPermission = false;
    for (const permission of permissions) {
      if (
        permission.resourceType === SecuredResourceType.CHATS &&
        permission.action === SecuredAction.READ
      ) {
        switch (permission.accessLevel) {
          case SecuredResourceAccessLevel.INSTANCE:
            hasPermission ||= true;
          case SecuredResourceAccessLevel.SELF:
            return (hasPermission ||= chat.userId === userId);
        }
      }
    }

    return hasPermission;
  }
}
