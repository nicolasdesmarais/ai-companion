import { clerkClient } from "@clerk/nextjs";
import { UpdateUserMetadataParams } from "./UpdateUserMetadataParams";

export class ClerkService {
  public async getUsersById(userIds: string[]) {
    if (userIds.length === 0) {
      return [];
    }

    return await clerkClient.users.getUserList({
      userId: userIds,
    });
  }

  public async updateUserMetadata(usedId: string, params: UpdateUserMetadataParams) {
    return await clerkClient.users.updateUserMetadata(usedId, params);
  }
}
