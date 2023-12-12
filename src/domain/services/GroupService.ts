import EmailUtils from "@/src/lib/emailUtils";
import prismadb from "@/src/lib/prismadb";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { clerkClient } from "@clerk/nextjs";
import { GroupAvailability, Prisma } from "@prisma/client";
import { GroupSecurityService } from "../../security/services/GroupSecurityService";
import { BadRequestError, EntityNotFoundError } from "../errors/Errors";
import { GroupEntity } from "../models/GroupEntity";
import {
  CreateGroupRequest,
  GroupDetailDto,
  GroupSummaryDto,
  UpdateGroupRequest,
} from "../ports/api/GroupsApi";
import { InvitationService } from "./InvitationService";

const groupSummarySelect: Prisma.GroupSelect = {
  id: true,
  createdAt: true,
  updatedAt: true,
  orgId: true,
  ownerUserId: true,
  name: true,
  availability: true,
};

const groupDetailSelect: Prisma.GroupSelect = {
  ...groupSummarySelect,
  users: {
    select: {
      createdAt: true,
      updatedAt: true,
      userId: true,
      email: true,
    },
  },
};

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
    authorizationContext: AuthorizationContext
  ): Promise<GroupSummaryDto[]> {
    const { orgId, userId } = authorizationContext;

    return await prismadb.group.findMany({
      select: groupSummarySelect,
      where: this.getGroupCriteria(orgId, userId),
    });
  }

  public async createGroup(
    authorizationContext: AuthorizationContext,
    createGroupRequest: CreateGroupRequest
  ): Promise<GroupDetailDto | null> {
    const { orgId, userId } = authorizationContext;
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

    const groupWithUsers = await prismadb.group.findUnique({
      select: groupDetailSelect,
      where: {
        id: group.id,
      }
    });

    return groupWithUsers;
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

    const groupPermissions = GroupSecurityService.getGroupPermissions(
      orgId,
      userId,
      existingGroup
    );

    let updatedGroup;
    if (groupPermissions.canUpdateGroup) {
      updatedGroup = await prismadb.group.update({
        where: {
          id: groupId,
        },
        data: {
          name: updateGroupRequest.name,
          availability: updateGroupRequest.availability,
        },
      });
    } else {
      updatedGroup = existingGroup;
    }

    if (updatedGroup.availability === GroupAvailability.EVERYONE) {
      //  Remove all explicit permissions
      await prismadb.groupUser.deleteMany({
        where: {
          groupId: groupId,
        },
      });
    } else if (
      updateGroupRequest.availability === GroupAvailability.RESTRICTED
    ) {
      // RESTRICTED availability. Create explicit permissions for users
      if (
        groupPermissions.canInviteUsersToGroup &&
        updateGroupRequest?.memberEmailsToAdd
      ) {
        await this.addUsersToGroup(
          orgId,
          userId,
          groupId,
          updateGroupRequest.memberEmailsToAdd
        );
      }
      if (
        groupPermissions.canRemoveUsersFromGroup &&
        updateGroupRequest?.memberEmailsToRemove
      ) {
        await this.removeUsersFromGroup(
          groupId,
          updateGroupRequest.memberEmailsToRemove
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
    const validEmails = EmailUtils.parseEmailCsv(emailsToAdd);
    if (validEmails.length === 0) {
      return;
    }

    const foundUserEmails = new Set<string>();
    const missingUserEmails = new Set<string>();
    const groupUsers: {
      groupId: string;
      userId: string | null;
      email: string;
    }[] = [];

    const clerkUserList = await clerkClient.users.getUserList({
      emailAddress: validEmails,
    });

    clerkUserList.forEach((clerkUser) => {
      for (const { emailAddress } of clerkUser.emailAddresses) {
        if (validEmails.includes(emailAddress)) {
          foundUserEmails.add(emailAddress);
          groupUsers.push({
            groupId,
            userId: clerkUser.id,
            email: emailAddress,
          });
        }
      }
    });

    validEmails.forEach((email) => {
      if (!foundUserEmails.has(email)) {
        missingUserEmails.add(email);
        groupUsers.push({
          groupId,
          userId: null,
          email,
        });
      }
    });

    await prismadb.groupUser.createMany({
      data: groupUsers,
      skipDuplicates: true,
    });

    // Invite users who were not found in Clerk
    const invitationService = new InvitationService();
    await invitationService.createOrganizationInvitationsFromEmails(
      orgId,
      createdByUserId,
      Array.from(missingUserEmails)
    );
  }

  private async removeUsersFromGroup(
    groupId: string,
    memberEmailsToRemove: string[]
  ) {
    await prismadb.groupUser.deleteMany({
      where: {
        groupId: groupId,
        email: {
          in: memberEmailsToRemove,
        },
      },
    });
  }

  public async populateGroupUserId(userId: string, email: string) {
    await prismadb.groupUser.updateMany({
      where: {
        email,
      },
      data: {
        userId,
      },
    });
  }

  public async deleteGroup(orgId: string, userId: string, groupId: string) {
    const existingGroup = await this.findGroupById(orgId, userId, groupId);
    if (!existingGroup) {
      throw new EntityNotFoundError("Group not found");
    }

    await prismadb.$transaction([
      prismadb.groupUser.deleteMany({
        where: {
          groupId: groupId,
        },
      }),
      prismadb.group.delete({
        where: {
          id: groupId,
        },
      }),
    ]);
  }

  public async leaveGroup(orgId: string, userId: string, groupId: string) {
    const existingGroup = await this.findGroupById(orgId, userId, groupId);
    if (!existingGroup) {
      throw new EntityNotFoundError("Group not found");
    }

    if (existingGroup.ownerUserId === userId) {
      throw new BadRequestError("Owner cannot leave group");
    }
    await prismadb.groupUser.deleteMany({
      where: {
        groupId: groupId,
        userId: userId,
      },
    });
  }

  public async updateAIGroups(aiId: string, groupIds: string[]) {
    await prismadb.groupAI.deleteMany({
      where: { aiId },
    });
    await prismadb.groupAI.createMany({
      data: groupIds.map((groupId) => {
        return {
          aiId,
          groupId,
        };
      }),
    });
  }
}

const groupService = new GroupService();
export default groupService;
