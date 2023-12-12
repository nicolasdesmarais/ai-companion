import { AISummaryDto } from "@/src/domain/ports/api/AIApi";
import { AI } from "@prisma/client";
import { AuthorizationContext } from "../models/AuthorizationContext";
import { SecuredAction } from "../models/SecuredAction";
import { SecuredResourceType } from "../models/SecuredResourceType";
import { BaseEntitySecurityService } from "./BaseEntitySecurityService";

export class AISecurityService {
  public static canUpdateAI(
    authorizationContext: AuthorizationContext,
    ai: AISummaryDto
  ) {
    return BaseEntitySecurityService.canUpdateEntity(
      authorizationContext,
      ai,
      SecuredResourceType.AI,
      SecuredAction.WRITE
    );
  }

  public static canDeleteAI(
    authorizationContext: AuthorizationContext,
    ai: AI
  ) {
    return this.canUpdateAI(authorizationContext, ai);
  }
}
