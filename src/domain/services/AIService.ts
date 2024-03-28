import {
  CreateAIRequest,
  ListAIsRequestParams,
  ListAIsRequestScope,
  ShareAIRequest,
  UpdateAIRequest,
} from "@/src/adapter-in/api/AIApi";
import { AIRepositoryImpl } from "@/src/adapter-out/repositories/AIRepositoryImpl";
import { BadRequestError } from "@/src/domain/errors/Errors";
import EmailUtils from "@/src/lib/emailUtils";
import prismadb from "@/src/lib/prismadb";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";
import { DataSourceSecurityService } from "@/src/security/services/DataSourceSecurityService";
import { clerkClient } from "@clerk/nextjs";
import { User } from "@clerk/nextjs/server";
import { SystemMessage } from "@langchain/core/messages";
import { ChatOpenAI } from "@langchain/openai";
import {
  AI,
  AIVisibility,
  DataSourceRefreshPeriod,
  DataSourceType,
  GroupAI,
  GroupAvailability,
  Prisma,
} from "@prisma/client";
import { AISecurityService } from "../../security/services/AISecurityService";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import { AIDetailDto, AIProfile } from "../models/AI";
import { AIModelOptions } from "../models/AIModel";
import { AIRepository } from "../ports/outgoing/AIRepository";
import aiModelService from "./AIModelService";
import dataSourceManagementService from "./DataSourceManagementService";
import groupService from "./GroupService";
import invitationService from "./InvitationService";

const openai = new ChatOpenAI({
  azureOpenAIApiKey: process.env.AZURE_GPT35_KEY,
  azureOpenAIApiVersion: "2023-05-15",
  azureOpenAIApiInstanceName: "appdirect-prod-ai-useast",
  azureOpenAIApiDeploymentName: "ai-prod-16k",
});

const listAIResponseSelect = (): Prisma.AISelect => ({
  id: true,
  createdAt: true,
  updatedAt: true,
  name: true,
  introduction: true,
  description: true,
  src: true,
  profile: true,
  orgId: true,
  userId: true,
  userName: true,
  categoryId: true,
  visibility: true,
  listInOrgCatalog: true,
  listInPublicCatalog: true,
  modelId: true,
  options: true,
  instructions: true,
  seed: true,
  groups: {
    select: {
      groupId: true,
    },
  },
  orgApprovals: {
    select: {
      id: true,
    },
  },
});

export class AIService {
  constructor(private aiRepository: AIRepository) {}

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
    authorizationContext: AuthorizationContext,
    aiId: string,
    request: ShareAIRequest
  ) {
    const { orgId, userId } = authorizationContext;

    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });
    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const canShareAI = AISecurityService.canUpdateAI(authorizationContext, ai);
    if (!canShareAI) {
      throw new ForbiddenError("Forbidden");
    }

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

  public async getById(
    authorizationContext: AuthorizationContext,
    aiId: string
  ): Promise<AIDetailDto> {
    const ai = await prismadb.aI.findFirst({
      select: listAIResponseSelect(),
      where: {
        id: aiId,
      },
    });
    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }
    const { userId } = authorizationContext;

    const isShared = await this.aiRepository.hasPermissionOnAI(aiId, userId);
    if (
      !(await AISecurityService.canReadAI(authorizationContext, ai, isShared))
    ) {
      throw new ForbiddenError(
        `User is not authorized to read AI with id=${aiId}`
      );
    }

    const canUpdateAI = AISecurityService.canUpdateAI(authorizationContext, ai);
    const messageCountPerAi: any[] = await this.getMessageCountPerAi([ai.id]);
    const ratingPerAi: any[] = await this.getRatingPerAi([ai.id]);

    const aiDto = this.mapToAIDto(
      ai,
      messageCountPerAi,
      ratingPerAi,
      isShared,
      canUpdateAI
    );
    return aiDto;
  }

  /**
   * Returns an AI by id if it is public.
   * @param aiId
   * @returns
   */
  public async findPublicAIById(aiId: string): Promise<AIDetailDto | null> {
    const ai = await prismadb.aI.findFirst({
      select: listAIResponseSelect(),
      where: {
        id: aiId,
      },
    });
    if (!ai) {
      return null;
    }

    if (!(await AISecurityService.canReadAI(null, ai, false))) {
      return null;
    }

    const messageCountPerAi: any[] = await this.getMessageCountPerAi([ai.id]);
    const ratingPerAi: any[] = await this.getRatingPerAi([ai.id]);

    const aiDto = this.mapToAIDto(
      ai,
      messageCountPerAi,
      ratingPerAi,
      false,
      false
    );
    return aiDto;
  }

  /**
   * Returns a list AIs which are public.
   * The list of AIs is further filtered down based on the provided scope
   * @param request
   * @returns
   */
  public async findPublicAIs(
    request: ListAIsRequestParams = {}
  ): Promise<AIDetailDto[]> {
    const { categoryId, search } = request;
    const whereCondition = { AND: [{}] };
    whereCondition.AND.push(this.getPublicCriteria());

    if (categoryId) {
      whereCondition.AND.push(this.getCategoryCriteria(categoryId));
    }

    if (search) {
      whereCondition.AND.push(this.getSearchCriteria(search));
    }

    const ais = await prismadb.aI.findMany({
      select: {
        ...listAIResponseSelect(),
      },
      where: whereCondition,
      orderBy: {
        createdAt: "desc",
      },
    });

    if (ais.length === 0) {
      return [];
    }

    const aiIds = ais.map((ai) => ai.id);

    const messageCountPerAi: any[] = await this.getMessageCountPerAi(aiIds);
    const ratingPerAi: any[] = await this.getRatingPerAi(aiIds);

    const result = ais.map((ai) => {
      return this.mapToAIDto(ai, messageCountPerAi, ratingPerAi, false);
    });

    if (request.sort === "newest") {
      return result;
    } else if (!request.sort || request.sort === "popularity") {
      return result.sort((a, b) => b.messageCount - a.messageCount);
    } else if (request.sort === "rating") {
      return result.sort((a, b) => b.rating - a.rating);
    }

    return result;
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
    authorizationContext: AuthorizationContext,
    request: ListAIsRequestParams
  ): Promise<AIDetailDto[]> {
    const scope = this.determineScope(authorizationContext, request.scope);
    const { orgId, userId } = authorizationContext;
    const { groupId, categoryId, approvedByOrg, search } = request;

    const whereCondition = { AND: [{}] };
    whereCondition.AND.push(this.getBaseWhereCondition(orgId, userId, scope));

    if (groupId) {
      if (scope === ListAIsRequestScope.INSTANCE_ORGANIZATION) {
        whereCondition.AND.push(this.getInstanceGroupCriteria(groupId));
      } else {
        whereCondition.AND.push(this.getGroupCriteria(orgId, groupId));
      }
    }
    if (categoryId) {
      whereCondition.AND.push(this.getCategoryCriteria(categoryId));
    }
    if (search) {
      whereCondition.AND.push(this.getSearchCriteria(search));
    }

    if (approvedByOrg !== null && approvedByOrg !== undefined) {
      whereCondition.AND.push(
        this.getApprovedByOrgCriteria(orgId, approvedByOrg)
      );
    }

    const ais = await prismadb.aI.findMany({
      select: {
        ...listAIResponseSelect(),
        chats: {
          where: {
            userId,
            isDeleted: false,
          },
        },
      },
      where: whereCondition,
      orderBy: {
        createdAt: "desc",
      },
    });

    if (ais.length === 0) {
      return [];
    }

    const aiShares = await this.getAISharesForUser(userId);

    const aiIds = ais.map((ai) => ai.id);

    const messageCountPerAi: any[] = await this.getMessageCountPerAi(aiIds);
    const ratingPerAi: any[] = await this.getRatingPerAi(aiIds);

    const result = ais.map((ai) => {
      const isShared = !!aiShares.find((a) => a.aiId === ai.id);
      return this.mapToAIDto(ai, messageCountPerAi, ratingPerAi, isShared);
    });

    if (request.sort === "newest") {
      return result;
    } else if (!request.sort || request.sort === "popularity") {
      return result.sort((a, b) => b.messageCount - a.messageCount);
    } else if (request.sort === "rating") {
      return result.sort((a, b) => b.rating - a.rating);
    }

    return result;
  }

  private determineScope(
    authorizationContext: AuthorizationContext,
    scope: ListAIsRequestScope | null | undefined
  ) {
    if (!scope) {
      return ListAIsRequestScope.ALL;
    }

    if (scope === ListAIsRequestScope.INSTANCE) {
      const hasInstanceAccess = BaseEntitySecurityService.hasPermission(
        authorizationContext,
        SecuredResourceType.AI,
        SecuredAction.READ,
        SecuredResourceAccessLevel.INSTANCE
      );
      if (!hasInstanceAccess) {
        return ListAIsRequestScope.ALL;
      }
    }

    return scope;
  }

  private async getMessageCountPerAi(aiIds: string[]) {
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
    return messageCountPerAi;
  }

  private async getRatingPerAi(aiIds: string[]) {
    const ratingPerAi: any[] = await prismadb.$queryRaw`
      SELECT
        r.ai_id as aiId,
        COUNT(*) as ratingCount,
        AVG(r.rating) as averageRating
      FROM
        ai_ratings as r
      WHERE
        r.ai_id IN (${Prisma.join(aiIds)})
      GROUP BY
        r.ai_id`;
    return ratingPerAi;
  }

  private mapToAIDto(
    ai: AI & { groups: GroupAI[] } & { orgApprovals: { id: number }[] },
    messageCountPerAi: any[],
    ratingPerAi: any[],
    isShared: boolean,
    forUpdate: boolean = false
  ): AIDetailDto {
    const aiCountRow = messageCountPerAi.find((m) => m.aiId === ai.id);
    const messageCount = aiCountRow ? Number(aiCountRow.messageCount) : 0;

    const ratingRow = ratingPerAi.find((r) => r.aiId === ai.id);
    const rating = ratingRow ? Number(ratingRow.averageRating) : 0;
    const ratingCount = ratingRow ? Number(ratingRow.ratingCount) : 0;

    const isApprovedByOrg = ai.orgApprovals.length > 0;

    const profile = ai.profile as unknown as AIProfile;

    const { options, ...aiWithoutOptions } = ai;
    let aiModelOptions: AIModelOptions;
    if (forUpdate || profile?.showPersonality) {
      aiModelOptions = options as unknown as AIModelOptions;
    } else {
      aiModelOptions = {} as AIModelOptions;
    }

    let filteredAi;
    const { modelId, instructions, visibility, ...aiWithoutCharacter } = ai;
    if (forUpdate || profile?.showCharacter) {
      filteredAi = ai;
    } else {
      filteredAi = { visibility, ...aiWithoutCharacter };
    }

    const { seed, ...aiWithoutSeed } = filteredAi;
    if (!forUpdate) {
      filteredAi = aiWithoutSeed;
    }

    const { orgApprovals, ...aiWithoutApprovals } = filteredAi;
    filteredAi = aiWithoutApprovals;

    const groupIds: string[] = ai.groups.map((groupAi) => groupAi.groupId);

    return {
      ...filteredAi,
      options: aiModelOptions,
      profile,
      messageCount,
      rating,
      ratingCount,
      isShared,
      isApprovedByOrg,
      groups: groupIds,
    };
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
        baseWhereCondition.AND.push(this.getOwnedByUserCriteria(orgId, userId));
        break;
      case ListAIsRequestScope.OWNED:
        baseWhereCondition = this.getOwnedByUserCriteria(orgId, userId);
        break;
      case ListAIsRequestScope.GROUP:
        baseWhereCondition = this.getUserGroupCriteria(orgId, userId);
        break;
      case ListAIsRequestScope.SHARED:
        baseWhereCondition = this.getSharedWithUserCriteria(orgId, userId);
        break;
      case ListAIsRequestScope.ORGANIZATION:
        baseWhereCondition = {
          OR: [
            this.getOrganizationCriteria(orgId),
            this.getUserGroupCriteria(orgId, userId),
          ],
        };
        break;
      case ListAIsRequestScope.PUBLIC:
        baseWhereCondition = this.getPublicCriteria();
        break;
      case ListAIsRequestScope.ALL:
        baseWhereCondition = { OR: [{}] };
        baseWhereCondition.OR.push(this.getOwnedByUserCriteria(orgId, userId));
        baseWhereCondition.OR.push(this.getUserGroupCriteria(orgId, userId));
        baseWhereCondition.OR.push(
          this.getSharedWithUserCriteria(orgId, userId)
        );
        baseWhereCondition.OR.push(this.getOrganizationCriteria(orgId));
        baseWhereCondition.OR.push(this.getPublicCriteria());
        break;
      case ListAIsRequestScope.INSTANCE:
        baseWhereCondition = { AND: [{}] };
        break;
      case ListAIsRequestScope.INSTANCE_ORGANIZATION:
        baseWhereCondition = this.getAllOrganizationCriteria();
        break;
      case ListAIsRequestScope.INSTANCE_NOT_VISIBLE:
        baseWhereCondition = { OR: [{}] };
        baseWhereCondition.OR.push(this.geOthersPrivateCriteria(orgId, userId));
        baseWhereCondition.OR.push(this.geOthersOrganizationCriteria(orgId));
        baseWhereCondition.OR.push(this.geOthersGroupCriteria(orgId, userId));
        break;
      case ListAIsRequestScope.INSTANCE_PRIVATE:
        baseWhereCondition = { visibility: AIVisibility.PRIVATE };
        break;
      case ListAIsRequestScope.ADMIN:
        baseWhereCondition = {
          OR: [this.getAllAdminCriteria(orgId), this.getPublicCriteria()],
        };
        break;
      case ListAIsRequestScope.ADMIN_ORGANIZATION:
        baseWhereCondition = this.getAllAdminOrganizationCriteria(orgId);
        break;
      case ListAIsRequestScope.ADMIN_PRIVATE:
        baseWhereCondition = { orgId, visibility: AIVisibility.PRIVATE };
        break;
      case ListAIsRequestScope.ADMIN_NOT_VISIBLE:
        baseWhereCondition = {
          AND: [
            {
              orgId,
              OR: [
                this.geOthersPrivateCriteria(orgId, userId),
                this.geOthersOrganizationCriteria(orgId),
                this.geOthersGroupCriteria(orgId, userId),
              ],
            },
          ],
        };
        break;
    }

    return baseWhereCondition;
  }

  private getOwnedByUserCriteria(orgId: string, userId: string) {
    return { orgId, userId };
  }

  private getUserGroupCriteria(orgId: string, userId: string) {
    return {
      visibility: {
        in: [AIVisibility.GROUP],
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
              {
                ownerUserId: userId,
              },
            ],
          },
        },
      },
    };
  }

  private getSharedWithUserCriteria(orgId: string, userId: string) {
    return {
      orgId,
      permissions: {
        some: {
          userId: userId,
        },
      },
    };
  }

  private geOthersGroupCriteria(orgId: string, userId: string) {
    return {
      visibility: AIVisibility.GROUP,
      groups: {
        some: {
          group: {
            OR: [
              {
                NOT: {
                  orgId,
                },
              },
              {
                users: {
                  some: {
                    NOT: {
                      userId: userId,
                    },
                  },
                },
                NOT: {
                  ownerUserId: userId,
                },
              },
            ],
          },
        },
      },
    };
  }

  private geOthersOrganizationCriteria(orgId: string) {
    return {
      visibility: AIVisibility.ORGANIZATION,
      NOT: {
        orgId,
      },
    };
  }

  private geOthersPrivateCriteria(orgId: string, userId: string) {
    return {
      visibility: AIVisibility.PRIVATE,
      NOT: this.getOwnedByUserCriteria(orgId, userId),
    };
  }

  private getOrganizationCriteria(orgId: string) {
    return {
      orgId,
      listInOrgCatalog: true,
      visibility: {
        in: [AIVisibility.ORGANIZATION, AIVisibility.ANYONE_WITH_LINK],
      },
    };
  }

  private getAllOrganizationCriteria() {
    return {
      visibility: {
        in: [AIVisibility.ORGANIZATION, AIVisibility.GROUP],
      },
    };
  }

  private getAllAdminCriteria(orgId: string) {
    return {
      orgId,
    };
  }

  private getAllAdminOrganizationCriteria(orgId: string) {
    return {
      orgId,
      visibility: {
        in: [AIVisibility.ORGANIZATION, AIVisibility.GROUP],
      },
    };
  }

  private getPublicCriteria() {
    return {
      listInPublicCatalog: true,
      visibility: {
        in: [AIVisibility.ANYONE_WITH_LINK],
      },
    };
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

  private getInstanceGroupCriteria(groupId: string) {
    return {
      groups: {
        some: {
          group: {
            id: groupId,
          },
        },
      },
    };
  }

  private getCategoryCriteria(categoryId: string) {
    return { categoryId: categoryId };
  }

  private getSearchCriteria(search: string) {
    const wildSearch = `*${search}*`;
    return {
      OR: [
        {
          name: {
            search: wildSearch,
          },
        },
        {
          userName: {
            search: wildSearch,
          },
        },
      ],
    };
  }

  private getApprovedByOrgCriteria(orgId: string, isApprovedByOrg: boolean) {
    if (isApprovedByOrg === true) {
      return {
        orgApprovals: {
          some: {
            orgId,
          },
        },
      };
    } else {
      return {
        orgApprovals: {
          none: {
            orgId,
          },
        },
      };
    }
  }

  public async createDataSourceAndAddToAI(
    authorizationContext: AuthorizationContext,
    aiId: string,
    name: string,
    type: DataSourceType,
    refreshPeriod: DataSourceRefreshPeriod,
    data: any
  ) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const canUpdateAI = AISecurityService.canUpdateAI(authorizationContext, ai);
    if (!canUpdateAI) {
      throw new ForbiddenError("Forbidden");
    }

    const dataSourceId = await dataSourceManagementService.initializeDataSource(
      authorizationContext,
      name,
      type,
      refreshPeriod,
      data
    );

    return await prismadb.aIDataSource.create({
      data: {
        aiId,
        dataSourceId,
      },
    });
  }

  public async addDatasourceToAI(
    authorizationContext: AuthorizationContext,
    aiId: string,
    dataSourceId: string
  ) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const canUpdateAI = AISecurityService.canUpdateAI(authorizationContext, ai);
    if (!canUpdateAI) {
      throw new ForbiddenError("Forbidden");
    }

    const dataSource = await prismadb.dataSource.findUnique({
      where: {
        id: dataSourceId,
      },
    });

    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    const canReadDataSource = DataSourceSecurityService.canReadDataSource(
      authorizationContext,
      dataSource
    );
    if (!canReadDataSource) {
      throw new ForbiddenError("Forbidden");
    }

    if (ai.orgId !== dataSource.orgId) {
      throw new BadRequestError(
        "DataSources must belong to the same org as the AI"
      );
    }

    return await prismadb.aIDataSource.create({
      data: {
        aiId,
        dataSourceId,
      },
    });
  }

  public async deleteAI(
    authorizationContext: AuthorizationContext,
    aiId: string
  ) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });
    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const canDeleteAI = AISecurityService.canDeleteAI(authorizationContext, ai);
    if (!canDeleteAI) {
      throw new ForbiddenError("Forbidden");
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
    return await prismadb.aIPermissions.updateMany({
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
    authorizationContext: AuthorizationContext,
    request: CreateAIRequest
  ) {
    const {
      userName,
      src,
      name,
      introduction,
      description,
      instructions,
      seed,
      categoryId,
      modelId,
      visibility,
      listInOrgCatalog,
      listInPublicCatalog,
      options,
      groups,
    } = request;

    if (!src || !name || !description || !instructions || !categoryId) {
      throw new BadRequestError("Missing required fields");
    }

    const { orgId, userId } = authorizationContext;

    const ai = await prismadb.aI.create({
      data: {
        categoryId,
        orgId,
        userId,
        userName,
        src,
        name,
        introduction,
        description,
        instructions,
        seed: seed ?? "",
        modelId,
        visibility,
        listInOrgCatalog,
        listInPublicCatalog,
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
    await this.createAssistant(ai);

    return ai;
  }

  /**
   * Updates an existing AI
   * @param request
   */
  public async updateAI(
    authorizationContext: AuthorizationContext,
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

    const canUpdateAI = AISecurityService.canUpdateAI(authorizationContext, ai);
    if (!canUpdateAI) {
      throw new ForbiddenError("Forbidden");
    }

    const {
      src,
      name,
      introduction,
      description,
      instructions,
      seed,
      categoryId,
      modelId,
      groups,
      visibility,
      listInOrgCatalog,
      listInPublicCatalog,
      options,
      profile,
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
        introduction,
        description,
        instructions,
        seed,
        modelId,
        visibility,
        listInOrgCatalog,
        listInPublicCatalog,
        options: options as any,
        profile: profile as any,
      },
    });

    await this.updateAIGroups(updatedAI, groups);
    await this.updateAssistant(ai, updatedAI);
    return updatedAI;
  }

  private async updateAIGroups(ai: AI, groupIds: string[]) {
    if (ai.visibility !== "GROUP") {
      await groupService.updateAIGroups(ai.id, []);
    } else if (groupIds.length > 0) {
      await groupService.updateAIGroups(ai.id, groupIds);
    }
  }

  private async createAssistant(ai: AI) {
    const assistantModel = aiModelService.getAssistantModelInstance(ai.modelId);
    if (!assistantModel) {
      return;
    }

    const externalId = await assistantModel.createAssistant({
      ai,
    });
    await prismadb.aI.update({
      where: { id: ai.id },
      data: {
        externalId,
      },
    });
  }

  private async updateAssistant(currentAI: AI, updatedAI: AI) {
    const existingExternalId = currentAI.externalId;
    if (!existingExternalId) {
      // Create a new assistant if it doesn't exist externally
      await this.createAssistant(updatedAI);
      return;
    }

    const shouldUpdateModel = currentAI.modelId !== updatedAI.modelId;
    if (shouldUpdateModel) {
      const newAssistantModel = aiModelService.getAssistantModelInstance(
        updatedAI.modelId
      );
      const existingAssistantModel = aiModelService.getAssistantModelInstance(
        currentAI.modelId
      );

      // Create a new assistant with the updated model
      if (newAssistantModel) {
        await this.createAssistant(updatedAI);
      }

      // Delete the old assistant associated with the existing model
      if (existingAssistantModel) {
        await existingAssistantModel.deleteAssistant(existingExternalId);
      }
    } else {
      // Update existing assistant without changing the model
      const assistantModel = aiModelService.getAssistantModelInstance(
        currentAI.modelId
      );
      if (assistantModel) {
        await assistantModel.updateAssistant({ ai: updatedAI });
      }
    }
  }

  public async rateAi(
    userId: string,
    aiId: string,
    rating: number,
    review: string = "",
    headline: string = ""
  ) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const existingRating = await prismadb.aIRating.findMany({
      take: 1,
      where: {
        userId,
        aiId,
      },
    });
    if (existingRating.length > 0) {
      await prismadb.aIRating.update({
        where: {
          id: existingRating[0].id,
        },
        data: {
          rating,
          review,
          headline,
        },
      });
    } else {
      await prismadb.aIRating.create({
        data: {
          aiId,
          userId,
          rating,
          review,
          headline,
        },
      });
    }
  }

  public async getUserAiRating(userId: string, aiId: string) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    return await prismadb.aIRating.findMany({
      take: 1,
      where: {
        userId,
        aiId,
      },
    });
  }

  public async getAllReviews(aiId: string) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    return await prismadb.aIRating.findMany({
      where: {
        aiId,
      },
    });
  }

  public async generate(prompt: string) {
    const response = await openai.call([new SystemMessage(prompt)]);
    return response.content;
  }

  public async generateAIProfile(
    authorizationContext: AuthorizationContext,
    aiId: string
  ) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const canUpdateAi = AISecurityService.canUpdateAI(authorizationContext, ai);
    if (!canUpdateAi) {
      throw new ForbiddenError("Forbidden");
    }

    const intro =
      "You are making a marketing profile for an AI chatbot. This AI will be part of many other AIs that are part of a larger AI marketplace. Call to action is to get people to try to talk to this AI.";

    const background = `for the following AI chatbot:  ${ai.name}, ${ai.description}. Here are more details for this AI: ${ai.instructions}`;

    const headline = await this.generate(
      `${intro} Create a short, one sentence headline ${background}`
    );

    const description = await this.generate(
      `${intro} Create one paragraph description ${background}`
    );

    const featureTitle = await this.generate(
      `${intro} Create one three word feature ${background}`
    );

    const featureDescription = await this.generate(
      `${intro} This AI has the following feature: ${featureTitle}. Give a one sentence description of this feature ${background}`
    );

    const aiProfile = {
      headline,
      description,
      features: [
        {
          title: featureTitle,
          description: featureDescription,
        },
      ],
    };

    return aiProfile;
  }

  public async getAISharesForUser(userId: string) {
    const aiShares = await prismadb.aIPermissions.findMany({
      where: {
        userId,
      },
    });

    return aiShares;
  }

  public async getAIShareForAIAndUser(aiId: string, userId: string) {
    const aiShare = await prismadb.aIPermissions.findMany({
      where: {
        aiId,
        userId,
      },
    });

    return aiShare;
  }

  public async approveAIForOrganization(
    authorizationContext: AuthorizationContext,
    aiId: string
  ) {
    const ai = await aiRepository.getById(aiId);

    const canApproveAI = AISecurityService.canApproveAIForOrg(
      authorizationContext,
      ai
    );
    if (!canApproveAI) {
      throw new ForbiddenError("Forbidden");
    }

    const { orgId } = authorizationContext;
    await aiRepository.approveAIForOrg(aiId, orgId);
  }

  public async revokeAIForOrganization(
    authorizationContext: AuthorizationContext,
    aiId: string
  ) {
    const ai = await aiRepository.getById(aiId);

    const canApproveAI = AISecurityService.canApproveAIForOrg(
      authorizationContext,
      ai
    );
    if (!canApproveAI) {
      throw new ForbiddenError("Forbidden");
    }

    const { orgId } = authorizationContext;
    await aiRepository.revokeAIForOrg(aiId, orgId);
  }
}

const aiRepository = new AIRepositoryImpl();
const aiService = new AIService(aiRepository);
export default aiService;
