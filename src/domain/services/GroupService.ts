import EmailUtils from "@/src/lib/emailUtils";
import prismadb from "@/src/lib/prismadb";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { clerkClient } from "@clerk/nextjs";
import { GroupAvailability, Prisma } from "@prisma/client";
import {
  CreateGroupRequest,
  UpdateGroupRequest,
} from "../../adapter-in/api/GroupsApi";
import { GroupSecurityService } from "../../security/services/GroupSecurityService";
import { BadRequestError, EntityNotFoundError } from "../errors/Errors";
import { GroupDetailDto, GroupSummaryDto } from "../models/Groups";
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

const organizationGroupCriteria = (orgId: string): Prisma.GroupWhereInput => {
  return {
    orgId,
  };
};

const groupCriteria = (
  orgId: string,
  userId: string
): Prisma.GroupWhereInput => {
  return {
    ...organizationGroupCriteria(orgId),
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
};

export class GroupService {
  /**
   * Finds a Group by ID.
   * Returns null if the group does not exist or is not visible to the user
   * @param authorizationContext
   * @param groupId
   * @returns
   */
  public async findGroupById(
    authorizationContext: AuthorizationContext,
    groupId: string
  ): Promise<GroupDetailDto | null> {
    const { orgId, userId } = authorizationContext;

    const hasInstanceGroupsAccess =
      GroupSecurityService.hasInstanceGroupsReadAccess(authorizationContext);

    let whereCondition: Prisma.GroupWhereUniqueInput = {
      id: groupId,
    };

    if (!hasInstanceGroupsAccess) {
      const hasAdminGroupsAccess =
        GroupSecurityService.hasAdminGroupsReadAccess(authorizationContext);

      const additionalCondition = hasAdminGroupsAccess
        ? organizationGroupCriteria(orgId)
        : groupCriteria(orgId, userId);

      whereCondition.AND = additionalCondition;
    }

    return await prismadb.group.findUnique({
      select: groupDetailSelect,
      where: whereCondition,
    });
  }

  /**
   * Returns the list of Groups which are visible to the user
   * @param authorizationContext
   * @returns
   */
  public async findGroupsByUser(
    authorizationContext: AuthorizationContext
  ): Promise<GroupSummaryDto[]> {
    const { orgId, userId } = authorizationContext;

    let groups: GroupSummaryDto[] = await prismadb.group.findMany({
      select: groupSummarySelect,
      where: groupCriteria(orgId, userId),
    });

    const hasInstanceGroupsAccess =
      GroupSecurityService.hasInstanceGroupsReadAccess(authorizationContext);
    const hasAdminGroupsAccess =
      GroupSecurityService.hasAdminGroupsReadAccess(authorizationContext);

    if (hasInstanceGroupsAccess) {
      const allGroups: GroupSummaryDto[] = await this.getInstanceGroups();
      allGroups.forEach((group) => {
        if (!groups.find((g) => g.id === group.id)) {
          group.notVisibleToMe = true;
        }
      });
      groups = allGroups;
    } else if (hasAdminGroupsAccess) {
      const allGroups: GroupSummaryDto[] = await this.getOrganizationGroups(
        orgId
      );
      allGroups.forEach((group) => {
        if (!groups.find((g) => g.id === group.id)) {
          group.notVisibleToMe = true;
        }
      });
      groups = allGroups;
    }

    return groups;
  }

  public async getInstanceGroups() {
    return await prismadb.group.findMany({
      select: groupSummarySelect,
    });
  }

  public async getOrganizationGroups(orgId: string) {
    return await prismadb.group.findMany({
      where: organizationGroupCriteria(orgId),
      select: groupSummarySelect,
    });
  }

  /**
   * Creates a new group
   * @param authorizationContext
   * @param createGroupRequest
   * @returns
   */
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
      },
    });

    return groupWithUsers;
  }

  public async updateGroup(
    authorizationContext: AuthorizationContext,
    groupId: string,
    updateGroupRequest: UpdateGroupRequest
  ): Promise<GroupDetailDto | null> {
    const { orgId, userId } = authorizationContext;
    const existingGroup = await this.findGroupById(
      authorizationContext,
      groupId
    );
    if (!existingGroup) {
      throw new EntityNotFoundError("Group not found");
    }

    const groupPermissions = GroupSecurityService.getGroupPermissions(
      authorizationContext,
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

    const groupWithUsers = await prismadb.group.findUnique({
      select: groupDetailSelect,
      where: {
        id: groupId,
      },
    });
    return groupWithUsers;
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
    return await prismadb.groupUser.updateMany({
      where: {
        email,
      },
      data: {
        userId,
      },
    });
  }

  /**
   * Deletes a group
   * @param authorizationContext
   * @param groupId
   */
  public async deleteGroup(
    authorizationContext: AuthorizationContext,
    groupId: string
  ): Promise<void> {
    const existingGroup = await this.findGroupById(
      authorizationContext,
      groupId
    );
    if (!existingGroup) {
      throw new EntityNotFoundError("Group not found");
    }

    await prismadb.$transaction([
      prismadb.groupAI.deleteMany({
        where: {
          groupId: groupId,
        },
      }),
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

  public async leaveGroup(
    authorizationContext: AuthorizationContext,
    groupId: string
  ): Promise<void> {
    const existingGroup = await this.findGroupById(
      authorizationContext,
      groupId
    );
    if (!existingGroup) {
      throw new EntityNotFoundError("Group not found");
    }

    const { userId } = authorizationContext;
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
