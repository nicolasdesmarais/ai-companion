import { AuthorizationContext } from "../models/AuthorizationContext";
import { SecuredAction } from "../models/SecuredAction";
import { SecuredResourceAccessLevel } from "../models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "../models/SecuredResourceType";
import { BaseEntitySecurityService } from "./BaseEntitySecurityService";

export class UsageSecurityService {
  public static canViewOrgUsage(
    authorizationContext: AuthorizationContext,
    orgId: string
  ) {
    const highestAccessLevel = BaseEntitySecurityService.getHighestAccessLevel(
      authorizationContext,
      SecuredResourceType.ORG_USAGE,
      SecuredAction.READ
    );

    if (!highestAccessLevel) {
      return false;
    }

    if (highestAccessLevel === SecuredResourceAccessLevel.INSTANCE) {
      return true;
    }

    return orgId === authorizationContext.orgId;
  }
}
