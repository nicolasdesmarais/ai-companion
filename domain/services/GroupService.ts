import prismadb from "@/lib/prismadb";
import { clerkClient } from "@clerk/nextjs";
import { GroupAvailability } from "@prisma/client";
import { GroupEntity } from "../entities/GroupEntity";
import { EntityNotFoundError } from "../errors/Errors";
import { CreateGroupRequest } from "../types/CreateGroupRequest";
import {
  CreateOrganizationInvitationRequest,
  OrganizationInvitation,
} from "../types/CreateOrganizationInvitationRequest";
import { UpdateGroupRequest } from "../types/UpdateGroupRequest";
import { Utilities } from "../util/utilities";
import { InvitationService } from "./InvitationService";

export class GroupService {
  private getGroupCriteria(orgId: string, userId: string) {
    return {
      orgId,
      OR: [
        { availability: GroupAvailability.EVERYONE },
        { ownerUserId: userId },
        {
          users: {
            some: { userId },
          },
        },
      ],
    };
  }

  public async findGroupById(
    orgId: string,
    userId: string,
    groupId: string
  ): Promise<GroupEntity | null> {
    return await prismadb.group.findUnique({
      where: {
        id: groupId,
        ...this.getGroupCriteria(orgId, userId),
      },
      include: { users: true },
    });
  }

  public async findGroupsByUser(
    orgId: string | undefined | null,
    userId: string
  ) {
    if (!orgId) {
      return [];
    }

    return await prismadb.group.findMany({
      where: this.getGroupCriteria(orgId, userId),
    });
  }

  public async createGroup(
    orgId: string,
    userId: string,
    createGroupRequest: CreateGroupRequest
  ) {
    const group = await prismadb.group.create({
      data: {
        orgId,
        ownerUserId: userId,
        name: createGroupRequest.name,
        availability: createGroupRequest.availability,
      },
    });

    if (createGroupRequest.availability === GroupAvailability.RESTRICTED) {
      // Create explicit permissions for users when group availability is SELECT
      this.addUsersToGroup(
        orgId,
        userId,
        group.id,
        createGroupRequest.memberEmails
      );
    }

    return group;
  }

  public async updateGroup(
    orgId: string,
    userId: string,
    groupId: string,
    updateGroupRequest: UpdateGroupRequest
  ) {
    console.log(
      "GroupService.updateGroup: " + JSON.stringify(updateGroupRequest)
    );

    const existingGroup = await this.findGroupById(orgId, userId, groupId);
    if (!existingGroup) {
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
      existingGroup.availability === GroupAvailability.RESTRICTED
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
      console.log("GroupService.updateGroup: RESTRICTED");
      // RESTRICTED availability. Create explicit permissions for users
      if (updateGroupRequest?.memberEmailsToAdd) {
        await this.addUsersToGroup(
          orgId,
          userId,
          groupId,
          updateGroupRequest.memberEmailsToAdd
        );
      }
      if (updateGroupRequest?.userIdsToRemove) {
        await this.removeUsersFromGroup(
          groupId,
          updateGroupRequest.userIdsToRemove
        );
      }
    }

    return await prismadb.group.findUnique({
      where: {
        id: groupId,
      },
      include: {
        users: true,
      },
    });
  }

  private async addUsersToGroup(
    orgId: string,
    createdByUserId: string,
    groupId: string,
    emailsToAdd: string
  ) {
    const validEmails = Utilities.parseEmailCsv(emailsToAdd);
    if (validEmails.length === 0) {
      return;
    }
    console.log("[GroupService.addUsersToGroup] Valid emails: " + validEmails);

    const userIdsToAdd: string[] = [];
    const foundUserEmails = new Set<string>();

    const clerkUserList = await clerkClient.users.getUserList({
      emailAddress: validEmails,
    });

    console.log(
      "[GroupService.addUsersToGroup] Fetched clerk users: ",
      JSON.stringify(clerkUserList)
    );

    clerkUserList.forEach((clerkUser) => {
      userIdsToAdd.push(clerkUser.id);
      clerkUser.emailAddresses.forEach((emailAddress) => {
        foundUserEmails.add(emailAddress.emailAddress);
      });
    });

    const groupUsers = userIdsToAdd.map((userId) => ({
      groupId,
      userId,
    }));

    console.log("[GroupService.addUsersToGroup] Group users: ", groupUsers);

    try {
      // const result = await prismadb.groupUser.createMany({
      //   data: groupUsers,
      // });
      // console.log(
      //   "[GroupService.addUsersToGroup] Group users result: ",
      //   JSON.stringify(result)
      // );
      for (const groupUser of groupUsers) {
        const createdGroupUser = await prismadb.groupUser.create({
          data: groupUser,
        });

        console.log(
          "[GroupService.addUsersToGroup] Created group user: " +
            JSON.stringify(createdGroupUser)
        );
      }
    } catch (err) {
      console.log("[GroupService.addUsersToGroup] Error: ", err);
    }

    // Invite users who were not found in Clerk
    this.inviteMissingUsers(
      orgId,
      createdByUserId,
      validEmails,
      foundUserEmails
    );
  }

  private async inviteMissingUsers(
    orgId: string,
    userId: string,
    requestedEmails: string[],
    foundEmails: Set<string>
  ) {
    const missingEmails = requestedEmails.filter(
      (email) => !foundEmails.has(email)
    );

    const orgInvitations: OrganizationInvitation[] = [];
    for (const email of missingEmails) {
      const invitation: OrganizationInvitation = {
        emailAddress: email,
        role: "basic_member",
      };
      orgInvitations.push(invitation);
    }

    const createInvitationRequest: CreateOrganizationInvitationRequest = {
      organizationId: orgId,
      inviterUserId: userId,
      invitations: orgInvitations,
    };

    const invitationService = new InvitationService();
    await invitationService.createOrganizationInvitations(
      createInvitationRequest
    );
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
