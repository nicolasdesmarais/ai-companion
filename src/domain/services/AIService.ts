import openAIAssistantModelAdapter from "@/src/adapter/ai-model/OpenAIAssistantModelAdapter";
import { BadRequestError } from "@/src/domain/errors/Errors";
import EmailUtils from "@/src/lib/emailUtils";
import prismadb from "@/src/lib/prismadb";
import { clerkClient } from "@clerk/nextjs";
import { User } from "@clerk/nextjs/server";
import { AI, AIVisibility, GroupAvailability, Prisma } from "@prisma/client";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import { CreateAIRequest, UpdateAIRequest } from "../ports/api/AIApi";
import {
  ListAIsRequestParams,
  ListAIsRequestScope,
} from "../ports/api/ListAIsRequestParams";
import { ShareAIRequest } from "../ports/api/ShareAIRequest";
import aiModelService from "./AIModelService";
import { AISecurityService } from "./AISecurityService";
import groupService from "./GroupService";
import invitationService from "./InvitationService";

export class AIService {
  public async findAIById(id: string) {
    return prismadb.aI.findUnique({
      where: {
        id: id,
      },
      include: {
        permissions: true,
      },
    });
  }

  public async shareAi(
    orgId: string,
    userId: string,
    aiId: string,
    request: ShareAIRequest
  ) {
    const ais = await prismadb.aI.findMany({
      where: {
        AND: [
          {
            id: aiId,
            userId,
            orgId,
          },
        ],
      },
    });
    if (ais.length === 0) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }
    const ai = ais[0];

    const validEmails = EmailUtils.parseEmailCsv(request.emails);
    if (validEmails.length === 0) {
      return;
    }

    const foundUserEmails = new Set<string>();
    const missingUserEmails = new Set<string>();
    const aiPermissions: {
      userId: string | null;
      aiId: string;
      email: string;
    }[] = [];

    const clerkUserList = await clerkClient.users.getUserList({
      emailAddress: validEmails,
    });

    clerkUserList.forEach((clerkUser) => {
      for (const { emailAddress } of clerkUser.emailAddresses) {
        if (validEmails.includes(emailAddress)) {
          foundUserEmails.add(emailAddress);
          aiPermissions.push({
            aiId: aiId,
            userId: clerkUser.id,
            email: emailAddress,
          });
        }
      }
    });

    validEmails.forEach((email) => {
      if (!foundUserEmails.has(email)) {
        missingUserEmails.add(email);
        aiPermissions.push({
          aiId: aiId,
          userId: null,
          email,
        });
      }
    });

    await prismadb.aIPermissions.createMany({
      data: aiPermissions,
      skipDuplicates: true,
    });

    // Invite users who were not found in Clerk
    try {
      if (orgId) {
        await invitationService.createOrganizationInvitationsFromEmails(
          orgId,
          userId,
          Array.from(missingUserEmails)
        );
      } else {
        await invitationService.createInvitations(
          Array.from(missingUserEmails)
        );
      }
    } catch (error) {
      console.error("Error creating invitations", error);
    }

    await this.sendAiSharedEmail(ai, clerkUserList);
  }

  private async sendAiSharedEmail(ai: AI, clerkUsers: User[]) {
    for (const clerkUser of clerkUsers) {
      if (!clerkUser.primaryEmailAddressId) {
        continue;
      }

      await clerkClient.emails.createEmail({
        fromEmailName: "AppDirect.ai",
        emailAddressId: clerkUser.primaryEmailAddressId,
        subject: "AI shared with you on AppDirect.ai",
        body: `${ai.userName} has shared a ${ai.name} with you on AppDirect.ai.`,
      });
    }
  }

  /**
   * Returns an AI by ID, only if it's visible to the given user and organization.
   * @param orgId
   * @param userId
   * @param aiId
   * @returns
   */
  public async findAIForUser(orgId: string, userId: string, aiId: string) {
    const whereCondition = { AND: [{}] };
    whereCondition.AND.push(
      this.getBaseWhereCondition(orgId, userId, ListAIsRequestScope.ALL)
    );
    whereCondition.AND.push({ id: aiId });

    return await prismadb.aI.findFirst({
      where: whereCondition,
    });
  }

  /**
   * Returns a list AIs which are visible to a given user.
   * The list of AIs is further filtered down based on the provided scope
   * @param orgId
   * @param userId
   * @param request
   * @returns
   */
  public async findAIsForUser(
    orgId: string,
    userId: string,
    request: ListAIsRequestParams
  ) {
    const scope = request.scope || ListAIsRequestScope.ALL;

    const whereCondition = { AND: [{}] };
    whereCondition.AND.push(this.getBaseWhereCondition(orgId, userId, scope));

    if (request.groupId) {
      whereCondition.AND.push(this.getGroupCriteria(orgId, request.groupId));
    }
    if (request.categoryId) {
      whereCondition.AND.push(this.getCategoryCriteria(request.categoryId));
    }
    if (request.search) {
      whereCondition.AND.push(this.getSearchCriteria(request.search));
    }

    const ais = await prismadb.aI.findMany({
      where: whereCondition,
      orderBy: {
        createdAt: "desc",
      },
    });

    if (ais.length === 0) {
      return [];
    }

    const aiIds = ais.map((ai) => ai.id);

    const messageCountPerAi: any[] = await prismadb.$queryRaw`
      SELECT
        c.ai_id as aiId,
        COUNT(*) as messageCount
      FROM
        chats as c
        INNER JOIN messages as m ON m.chat_id = c.id
      WHERE
      c.is_deleted = false AND
      c.ai_id IN (${Prisma.join(aiIds)})
      GROUP BY
        c.ai_id`;

    const result = ais.map((ai) => {
      const aiCountRow = messageCountPerAi.find((m) => m.aiId === ai.id);
      const messageCount = aiCountRow ? Number(aiCountRow.messageCount) : 0;

      return {
        ...ai,
        messageCount,
      };
    });

    return result;
  }

  private getBaseWhereCondition(
    orgId: string,
    userId: string,
    scope: ListAIsRequestScope
  ) {
    let baseWhereCondition;

    switch (scope) {
      case ListAIsRequestScope.PRIVATE:
        baseWhereCondition = { AND: [{}] };
        baseWhereCondition.AND.push({ visibility: AIVisibility.PRIVATE });
        baseWhereCondition.AND.push(this.getOwnedByUserCriteria(userId));
        break;
      case ListAIsRequestScope.OWNED:
        baseWhereCondition = this.getOwnedByUserCriteria(userId);
        break;
      case ListAIsRequestScope.GROUP:
        baseWhereCondition = this.getUserGroupCriteria(orgId, userId);
        break;
      case ListAIsRequestScope.SHARED:
        baseWhereCondition = this.getSharedWithUserCriteria(userId);
        break;
      case ListAIsRequestScope.ORGANIZATION:
        baseWhereCondition = this.getOrganizationCriteria(orgId);
        break;
      case ListAIsRequestScope.PUBLIC:
        baseWhereCondition = this.getPublicCriteria();
        break;
      case ListAIsRequestScope.ALL:
        baseWhereCondition = { OR: [{}] };
        baseWhereCondition.OR.push(this.getOwnedByUserCriteria(userId));
        baseWhereCondition.OR.push(this.getUserGroupCriteria(orgId, userId));
        baseWhereCondition.OR.push(this.getSharedWithUserCriteria(userId));
        baseWhereCondition.OR.push(this.getOrganizationCriteria(orgId));
        baseWhereCondition.OR.push(this.getPublicCriteria());
        break;
    }

    return baseWhereCondition;
  }

  private getOwnedByUserCriteria(userId: string) {
    return { userId: userId };
  }

  private getUserGroupCriteria(orgId: string, userId: string) {
    return {
      visibility: {
        in: [AIVisibility.GROUP, AIVisibility.PUBLIC],
      },
      groups: {
        some: {
          group: {
            orgId: orgId,
            OR: [
              { availability: GroupAvailability.EVERYONE },
              {
                users: {
                  some: {
                    userId: userId,
                  },
                },
              },
            ],
          },
        },
      },
    };
  }

  private getSharedWithUserCriteria(userId: string) {
    return {
      permissions: {
        some: {
          userId: userId,
        },
      },
    };
  }

  private getOrganizationCriteria(orgId: string) {
    return {
      orgId,
      visibility: AIVisibility.ORGANIZATION,
    };
  }

  private getPublicCriteria() {
    return { visibility: AIVisibility.PUBLIC };
  }

  private getGroupCriteria(orgId: string, groupId: string) {
    return {
      groups: {
        some: {
          group: {
            id: groupId,
            orgId: orgId,
          },
        },
      },
    };
  }

  private getCategoryCriteria(categoryId: string) {
    return { categoryId: categoryId };
  }

  private getSearchCriteria(search: string) {
    return {
      OR: [
        {
          name: {
            search: search,
          },
        },
        {
          userName: {
            search: search,
          },
        },
      ],
    };
  }

  public createAIDataSource(aiId: string, dataSourceId: string) {
    return prismadb.aIDataSource.create({
      data: {
        aiId,
        dataSourceId,
      },
    });
  }

  public async deleteAI(orgId: string, userId: string, aiId: string) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
        orgId,
        userId,
      },
    });
    if (!ai) {
      throw new EntityNotFoundError(
        `AI with id=${aiId} not found, for user=${userId} and org=${orgId}`
      );
    }

    await prismadb.$transaction(async (tx) => {
      await prismadb.aIDataSource.deleteMany({
        where: { aiId },
      });

      await prismadb.aIPermissions.deleteMany({
        where: { aiId },
      });

      await prismadb.aI.delete({
        where: { id: aiId },
      });
    });
  }

  public async populateAiPermissionsUserId(userId: string, email: string) {
    await prismadb.aIPermissions.updateMany({
      where: {
        email,
      },
      data: {
        userId,
      },
    });
  }

  /**
   * Creates a new AI
   * @param orgId
   * @param userId
   * @param request
   * @returns
   */
  public async createAI(
    orgId: string,
    userId: string,
    request: CreateAIRequest
  ) {
    const {
      userName,
      src,
      name,
      description,
      instructions,
      seed,
      categoryId,
      modelId,
      visibility,
      options,
      groups,
    } = request;

    if (!src || !name || !description || !instructions || !categoryId) {
      throw new BadRequestError("Missing required fields");
    }

    const ai = await prismadb.aI.create({
      data: {
        categoryId,
        orgId,
        userId,
        userName,
        src,
        name,
        description,
        instructions,
        seed: seed ?? "",
        modelId,
        visibility,
        options: options as any,
      },
      include: {
        dataSources: {
          include: {
            dataSource: true,
          },
        },
        groups: true,
      },
    });

    await this.updateAIGroups(ai, groups);
    await this.createOrUpdateExternalAI(ai);

    return ai;
  }

  /**
   * Updates an existing AI
   * @param request
   */
  public async updateAI(
    orgId: string,
    userId: string,
    aiId: string,
    request: UpdateAIRequest
  ) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const canUpdateAI = AISecurityService.canUpdateAI(orgId, userId, ai);
    if (!canUpdateAI) {
      throw new ForbiddenError("Forbidden");
    }

    const {
      src,
      name,
      description,
      instructions,
      seed,
      categoryId,
      modelId,
      groups,
      visibility,
      options,
    } = request;

    if (!src || !name || !description || !instructions || !categoryId) {
      throw new BadRequestError("Missing required fields");
    }

    const updatedAI = await prismadb.aI.update({
      where: {
        id: aiId,
      },
      include: {
        dataSources: {
          include: {
            dataSource: true,
          },
        },
        groups: true,
      },
      data: {
        categoryId,
        src,
        name,
        description,
        instructions,
        seed,
        modelId,
        visibility,
        options: options as any,
      },
    });

    await this.updateAIGroups(updatedAI, groups);
    await this.createOrUpdateExternalAI(updatedAI);
    return updatedAI;
  }

  private async updateAIGroups(ai: AI, groupIds: string[]) {
    if (ai.visibility !== "GROUP") {
      await groupService.updateAIGroups(ai.id, []);
    } else if (groupIds.length > 0) {
      await groupService.updateAIGroups(ai.id, groupIds);
    }
  }

  private async createOrUpdateExternalAI(ai: AI) {
    const aiModel = await aiModelService.findAIModelById(ai.modelId);
    if (!aiModel) {
      return;
    }

    // Hardcoded check for now
    if (aiModel.id === "gpt-4-assistant") {
      if (!ai.externalId) {
        const externalId = await openAIAssistantModelAdapter.createExternalAI(
          ai,
          aiModel
        );
        await prismadb.aI.update({
          where: { id: ai.id },
          data: {
            externalId,
          },
        });
      }
    }
  }
}

const aiService = new AIService();
export default aiService;
