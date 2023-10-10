import prismadb from "@/lib/prismadb";
import { clerkClient } from "@clerk/nextjs";
import { GroupAvailability } from "@prisma/client";
import { EntityNotFoundError } from "../errors/Errors";
import { CreateGroupRequest } from "../types/CreateGroupRequest";
import {
  CreateOrganizationInvitationRequest,
  OrganizationInvitation,
} from "../types/CreateOrganizationInvitationRequest";
import { UpdateGroupRequest } from "../types/UpdateGroupRequest";
import { Utilities } from "../util/utilities";
import { InvitationService } from "./InvitationService";

const EMAIL_REGEX = /^\S+@\S+\.\S+$/;

export class GroupService {
  private getGroupCriteria(orgId: string, userId: string) {
    return {
      orgId: orgId,
      OR: [
        { availability: GroupAvailability.EVERYONE },
        {
          users: {
            some: { userId: userId },
          },
        },
      ],
    };
  }

  public async findGroupById(orgId: string, userId: string, groupId: string) {
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
        orgId: orgId,
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
        createGroupRequest.memberEmails,
        true
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
      // Availability switching from EVERYONE to RESTRICTED. Create explicit permissions for users
      if (updateGroupRequest?.memberEmailsToAdd) {
        this.addUsersToGroup(
          orgId,
          userId,
          groupId,
          updateGroupRequest.memberEmailsToAdd,
          false
        );
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
    orgId: string,
    createdByUserId: string,
    groupId: string,
    emailsToAdd: string,
    includeCreator: boolean = false
  ) {
    const validEmails = Utilities.parseEmailCsv(emailsToAdd);
    const userIdsToAdd: string[] = [];
    if (includeCreator) {
      userIdsToAdd.push(createdByUserId);
    }
    const foundUserEmails = new Set<string>();

    if (validEmails.length > 0) {
      const clerkUserList = await clerkClient.users.getUserList({
        emailAddress: validEmails,
      });

      clerkUserList.forEach((clerkUser) => {
        userIdsToAdd.push(clerkUser.id);
        clerkUser.emailAddresses.forEach((emailAddress) => {
          foundUserEmails.add(emailAddress.emailAddress);
        });
      });
    }

    const groupUsers = userIdsToAdd.map((userId) => ({
      groupId: groupId,
      userId: userId,
    }));

    await prismadb.groupUser.createMany({ data: groupUsers });

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
