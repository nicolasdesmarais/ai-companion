import { AISummaryDto } from "@/src/domain/models/AI";
import { AI, AIVisibility } from "@prisma/client";
import { AuthorizationContext } from "../models/AuthorizationContext";
import { SecuredAction } from "../models/SecuredAction";
import { SecuredResourceAccessLevel } from "../models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "../models/SecuredResourceType";
import { BaseEntitySecurityService } from "./BaseEntitySecurityService";

export class AISecurityService {
  public static canReadAI(
    authorizationContext: AuthorizationContext,
    ai: AISummaryDto
  ) {
    const highestAccessLevel = BaseEntitySecurityService.getHighestAccessLevel(
      authorizationContext,
      SecuredResourceType.AI,
      SecuredAction.READ
    );
    if (!highestAccessLevel) {
      return false;
    }

    if (
      highestAccessLevel === SecuredResourceAccessLevel.INSTANCE ||
      ai.visibility === AIVisibility.PUBLIC ||
      ai.visibility === AIVisibility.UNLISTED
    ) {
      // Public & Unlisted AIs are always readable
      return true;
    }

    const { orgId, userId } = authorizationContext;
    if (ai.orgId !== orgId) {
      return false;
    }

    return (
      highestAccessLevel === SecuredResourceAccessLevel.ORGANIZATION ||
      ai.visibility === AIVisibility.ORGANIZATION ||
      ai.userId === userId
    );
  }

  public static canUpdateAI(
    authorizationContext: AuthorizationContext,
    ai: AISummaryDto
  ) {
    return BaseEntitySecurityService.hasPermissionOnEntity(
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

  public static canApproveAIForOrg(
    authorizationContext: AuthorizationContext,
    ai: AISummaryDto
  ) {
    if (
      BaseEntitySecurityService.hasPermission(
        authorizationContext,
        SecuredResourceType.AI,
        SecuredAction.WRITE,
        SecuredResourceAccessLevel.INSTANCE
      )
    ) {
      return true;
    }

    const hasOrgAccess = BaseEntitySecurityService.hasPermission(
      authorizationContext,
      SecuredResourceType.AI,
      SecuredAction.WRITE,
      SecuredResourceAccessLevel.ORGANIZATION
    );
    return hasOrgAccess && ai.orgId === authorizationContext.orgId;
  }

  public static hasInstanceReadAccess(
    authorizationContext: AuthorizationContext
  ) {
    return BaseEntitySecurityService.hasPermission(
      authorizationContext,
      SecuredResourceType.AI,
      SecuredAction.READ,
      SecuredResourceAccessLevel.INSTANCE
    );
  }
}
