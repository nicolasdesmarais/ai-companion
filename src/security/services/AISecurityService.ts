import { AISummaryDto } from "@/src/domain/models/AI";
import { AI, AIVisibility } from "@prisma/client";
import { AuthorizationContext } from "../models/AuthorizationContext";
import { SecuredAction } from "../models/SecuredAction";
import { SecuredResourceAccessLevel } from "../models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "../models/SecuredResourceType";
import { BaseEntitySecurityService } from "./BaseEntitySecurityService";

export class AISecurityService {
  public static async canReadAI(
    authorizationContext: AuthorizationContext | null,
    ai: AISummaryDto,
    hasPermission: boolean | ((ai: string, userId: string) => Promise<boolean>)
  ) {
    if (ai.visibility === AIVisibility.ANYONE_WITH_LINK) {
      //AIs with "Anyone with link" visibility  are always readable
      return true;
    }

    if (!authorizationContext) {
      return false;
    }

    const highestAccessLevel = BaseEntitySecurityService.getHighestAccessLevel(
      authorizationContext,
      SecuredResourceType.AI,
      SecuredAction.READ
    );
    if (!highestAccessLevel) {
      return false;
    }

    if (highestAccessLevel === SecuredResourceAccessLevel.INSTANCE) {
      // All AIs are visible with Instance access
      return true;
    }

    // Authorization context org must match AI org for remaining scenarios
    const { orgId, userId } = authorizationContext;
    if (ai.orgId !== orgId) {
      return false;
    }

    // User has org admin permission, or AI is org-visible, or user is AI owner
    if (
      highestAccessLevel === SecuredResourceAccessLevel.ORGANIZATION ||
      ai.visibility === AIVisibility.ORGANIZATION ||
      ai.userId === userId
    ) {
      return true;
    }

    // Check if user has explicit permission to view AI
    if (typeof hasPermission === "boolean") {
      return hasPermission;
    } else {
      return await hasPermission(ai.id, userId);
    }
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
