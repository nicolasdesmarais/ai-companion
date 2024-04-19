import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";
import {BaseEntitySecurityService} from "@/src/security/services/BaseEntitySecurityService";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {ForbiddenError} from "cohere-ai/api";
import {clerkClient} from "@clerk/nextjs";
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
            SecuredResourceAccessLevel.SELF
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
        userId: string
    ) : Promise<User> {
        return await clerkClient.users.getUser(userId);
    }
}

export const clerkService = new ClerkService();