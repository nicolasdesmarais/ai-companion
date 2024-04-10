import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";
import {BaseEntitySecurityService} from "@/src/security/services/BaseEntitySecurityService";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {ForbiddenError} from "cohere-ai/api";
import {clerkClient} from "@clerk/nextjs";
import {AISecurityService} from "@/src/security/services/AISecurityService";
import {User} from "@clerk/nextjs/server";

export interface UserMetaDataInterface {
    value: string;
    key: string;
}

export class ClerkService {
    public async updateUserMetadata(
        authorizationContext: AuthorizationContext,
        userId: string,
        sortKeyValue: UserMetaDataInterface
    ) {
        const hasWritePermission : boolean = BaseEntitySecurityService.hasPermission(
            authorizationContext,
            SecuredResourceType.AI,
            SecuredAction.WRITE,
            SecuredResourceAccessLevel.INSTANCE
        );

        if (!hasWritePermission) {
            throw new ForbiddenError("Forbidden");
        }

        const data: User = await clerkClient.users.updateUserMetadata(userId, {
            publicMetadata: { [sortKeyValue.key]: sortKeyValue.value },
        });
        return data;
    }

    public async getClerkUser(
        authorizationContext: AuthorizationContext,
        userId: string
    ) : Promise<User> {
        const hasReadPermission = AISecurityService.hasInstanceReadAccess(authorizationContext);
        if (!hasReadPermission) {
            throw new ForbiddenError("Forbidden");
        }

        const user : User = await clerkClient.users.getUser(userId);
        return user;
    }
}

export const clerkService = new ClerkService();