import { Permission } from "../models/Permission";
import { availableApiResourceTypes } from "../models/SecuredResourceType";

export class SecurityService {
  /**
   * Returns a list of all available for the given user
   * which can be used for API keys.
   * @param userPermissions
   * @returns
   */
  public getAvailableApiPermissions(
    userPermissions: Permission[]
  ): Permission[] {
    const availablePermissions = userPermissions.filter((permission) => {
      return availableApiResourceTypes.includes(permission.resourceType);
    });

    return availablePermissions;
  }
}

const securityService = new SecurityService();
export default securityService;
