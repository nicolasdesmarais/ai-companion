import prismadb from "@/lib/prismadb";
import { clerkClient } from "@clerk/nextjs";
import { SignedInAuthObject, SignedOutAuthObject } from "@clerk/nextjs/server";
import { GroupAvailability } from "@prisma/client";
import { EntityNotFoundError, UnauthorizedError } from "../errors/Errors";
import { CreateGroupRequest } from "../types/CreateGroupRequest";
import { UpdateGroupRequest } from "../types/UpdateGroupRequest";

export class GroupService {
  public async findGroupById(groupId: string, orgId: string, userId: string) {
    return await prismadb.group.findUnique({
      where: {
        id: groupId,
        orgId: orgId,
        OR: [
          {
            availability: GroupAvailability.EVERYONE,
          },
          {
            users: {
              some: {
                userId: userId,
              },
            },
          },
        ],
      },
      include: {
        users: true,
      },
    });
  }

  public async findGroupsByUser(
    auth: SignedInAuthObject | SignedOutAuthObject
  ) {
    if (!auth?.userId) {
      throw new UnauthorizedError("Unauthorized");
    }
    if (!auth?.orgId) {
      return [];
    }

    return await prismadb.group.findMany({
      where: this.getSecurityCriteria(auth.orgId, auth.userId),
    });
  }

  private getSecurityCriteria(orgId: string, userId: string) {
    return {
      orgId: orgId,
      OR: [
        {
          availability: GroupAvailability.EVERYONE,
        },
        {
          users: {
            some: {
              userId: userId,
            },
          },
        },
      ],
    };
  }

  public async createGroup(
    auth: SignedInAuthObject | SignedOutAuthObject,
    createGroupRequest: CreateGroupRequest
  ) {
    if (!auth?.userId || !auth?.orgId) {
      throw new UnauthorizedError("Unauthorized");
    }

    const group = await prismadb.group.create({
      data: {
        orgId: auth.orgId,
        name: createGroupRequest.name,
        availability: createGroupRequest.availability,
      },
    });

    if (createGroupRequest.availability === GroupAvailability.RESTRICTED) {
      // Create explicit permissions for users when group availability is SELECT
      this.addUsersToGroup(
        group.id,
        createGroupRequest.memberEmails,
        auth.userId
      );
    }

    return group;
  }

  public async updateGroup(
    groupId: string,
    orgId: string,
    userId: string,
    updateGroupRequest: UpdateGroupRequest
  ) {
    const group = await this.findGroupById(groupId, orgId, userId);
    if (!group) {
      throw new EntityNotFoundError("Group not found");
    }

    await prismadb.group.update({
      where: {
        id: groupId,
      },
      data: {
        name: updateGroupRequest.name,
        availability: updateGroupRequest.availability,
      },
    });

    if (
      updateGroupRequest.availability === GroupAvailability.EVERYONE &&
      group.availability === GroupAvailability.RESTRICTED
    ) {
      // Availability switching from RESTRICTED to EVERYONE. Remove all explicit permissions
      await prismadb.groupUser.deleteMany({
        where: {
          groupId: groupId,
        },
      });
    } else if (
      updateGroupRequest.availability === GroupAvailability.RESTRICTED
    ) {
      // Availability switching from EVERYONE to RESTRICTED. Create explicit permissions for users
      if (updateGroupRequest?.memberEmailsToAdd) {
        this.addUsersToGroup(groupId, updateGroupRequest.memberEmailsToAdd);
      }
      if (updateGroupRequest?.userIdsToRemove) {
        this.removeUsersFromGroup(groupId, updateGroupRequest.userIdsToRemove);
      }
    }

    return prismadb.group.findUnique({
      where: {
        id: groupId,
      },
      include: {
        users: true,
      },
    });
  }

  private async addUsersToGroup(
    groupId: string,
    emailsToAdd: string,
    createdByUserId?: string
  ) {
    const memberEmailsArray = emailsToAdd
      .split(",")
      .map((email) => email.trim());
    const uniqueMemberEmailsSet = new Set(memberEmailsArray);

    const clerkUserList = await clerkClient.users.getUserList({
      emailAddress: Array.from(uniqueMemberEmailsSet),
    });

    const userIds = clerkUserList.map((user) => user.id);
    if (createdByUserId) {
      userIds.push(createdByUserId);
    }

    const groupUsers = userIds.map((userId) => ({
      groupId: groupId,
      userId: userId,
    }));

    await prismadb.groupUser.createMany({ data: groupUsers });
  }

  private async removeUsersFromGroup(
    groupId: string,
    userIdsToRemove: string[]
  ) {
    await prismadb.groupUser.deleteMany({
      where: {
        groupId: groupId,
        userId: {
          in: userIdsToRemove,
        },
      },
    });
  }
}
