import { clerkClient } from "@clerk/nextjs";

export class ClerkService {
  public async getUsersById(userIds: string[]) {
    if (userIds.length === 0) {
      return [];
    }

    return await clerkClient.users.getUserList({
      userId: userIds,
    });
  }
}
