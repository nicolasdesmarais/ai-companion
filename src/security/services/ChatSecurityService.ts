import { ChatSummaryDto } from "@/src/domain/models/Chats";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { BaseEntitySecurityService } from "./BaseEntitySecurityService";

export class ChatSecurityService {
  /**
   * Returns true if the user has permission to read the specified chat.
   * @param authorizationContext
   * @param chat
   * @returns
   */

  public static canReadChat(
    authorizationContext: AuthorizationContext,
    chat: ChatSummaryDto
  ): boolean {
    return BaseEntitySecurityService.hasPermissionOnEntity(
      authorizationContext,
      chat,
      SecuredResourceType.CHATS,
      SecuredAction.READ
    );
  }
}
