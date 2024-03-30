import { clerkClient } from "@clerk/nextjs";

export interface UpdateUserMetadataParams {
    unsafeMetadata?: Record<string, unknown>;
}

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
      return clerkClient.users.updateUserMetadata(usedId, params);
  }

  public async getUserMetadata(userId: string) {
      const user = await clerkClient.users.getUser(userId);
      return user.unsafeMetadata;
  }
}

const clerkService = new ClerkService();
export default clerkService;