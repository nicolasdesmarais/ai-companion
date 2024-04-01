import { clerkClient } from "@clerk/nextjs";
import {User} from "@clerk/nextjs/server";

export type UserMetadataParams = {
    publicMetadata?: UserPublicMetadata;
    privateMetadata?: UserPrivateMetadata;
    unsafeMetadata?: UserUnsafeMetadata;
};

export class ClerkService {
  public async getUsersById(userIds: string[]) {
    if (userIds.length === 0) {
      return [];
    }

    return await clerkClient.users.getUserList({
      userId: userIds,
    });
  }

  public async updateUserMetadata(usedId: string, metaDataParams: UserMetadataParams) : Promise<User> {
      try {
          return await clerkClient.users.updateUserMetadata(usedId, metaDataParams);
      } catch (error) {
          console.error("Error at MetaDataUpdate: "+error);
          throw error;
      }
  }

  public async getUserMetadata(userId: string) {
      const user = await clerkClient.users.getUser(userId);
      return user.unsafeMetadata;
  }
}

const clerkService = new ClerkService();
export default clerkService;