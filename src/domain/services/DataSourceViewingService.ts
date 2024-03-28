import { AIRepositoryImpl } from "@/src/adapter-out/repositories/AIRepositoryImpl";
import { DataSourceRepositoryImpl } from "@/src/adapter-out/repositories/DataSourceRepositoryImpl";
import prismadb from "@/src/lib/prismadb";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { AISecurityService } from "@/src/security/services/AISecurityService";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";
import { KnowledgeIndexStatus } from "@prisma/client";
import { ForbiddenError } from "../errors/Errors";
import { DataSourceDto, DataSourceFilter } from "../models/DataSources";
import { AIRepository } from "../ports/outgoing/AIRepository";
import { DataSourceRepository } from "../ports/outgoing/DataSourceRepository";

export class DataSourceViewingService {
  constructor(
    private aiRepository: AIRepository,
    private dataSourceRepository: DataSourceRepository
  ) {}

  public async getById(dataSourceId: string): Promise<DataSourceDto> {
    return await this.dataSourceRepository.getById(dataSourceId);
  }

  /**
   * Returns a list of all data sources the user has access to.
   * @param authorizationContext
   * @returns
   */
  public async listDataSources(
    authorizationContext: AuthorizationContext,
    filter?: DataSourceFilter
  ): Promise<DataSourceDto[]> {
    const highestAccessLevel = BaseEntitySecurityService.getHighestAccessLevel(
      authorizationContext,
      SecuredResourceType.DATA_SOURCES,
      SecuredAction.READ
    );
    if (!highestAccessLevel) {
      throw new ForbiddenError("Forbidden");
    }

    return await this.listDataSourcesByLevel(
      authorizationContext,
      highestAccessLevel,
      filter
    );
  }

  /**
   * Returns a list of all data sources for the specified access level.
   * @param authorizationContext
   * @returns
   */
  public async listDataSourcesByLevel(
    authorizationContext: AuthorizationContext,
    level: SecuredResourceAccessLevel,
    filter?: DataSourceFilter
  ): Promise<DataSourceDto[]> {
    const hasPermission = BaseEntitySecurityService.hasPermission(
      authorizationContext,
      SecuredResourceType.DATA_SOURCES,
      SecuredAction.READ,
      level
    );
    if (!hasPermission) {
      throw new ForbiddenError("Forbidden");
    }
    const { orgId, userId } = authorizationContext;
    switch (level) {
      case SecuredResourceAccessLevel.INSTANCE:
        return await this.dataSourceRepository.findAll(filter);
      case SecuredResourceAccessLevel.ORGANIZATION:
        return await this.dataSourceRepository.findByOrgId(orgId, filter);
      case SecuredResourceAccessLevel.SELF:
        return await this.dataSourceRepository.findByOrgIdAndUserId(
          orgId,
          userId,
          filter
        );
    }
  }

  /**
   * Returns a list of all data sources for the specified AI.
   * @param authorizationContext
   * @param aiId
   * @returns
   */
  public async listAIDataSources(
    authorizationContext: AuthorizationContext,
    aiId: string
  ): Promise<DataSourceDto[]> {
    const ai = await this.aiRepository.getById(aiId);

    const canReadAi = AISecurityService.canReadAI(
      authorizationContext,
      ai,
      this.aiRepository.hasPermissionOnAI
    );
    if (!canReadAi) {
      throw new ForbiddenError(
        `User is not authorized to read AI with id=${aiId}`
      );
    }

    return await this.dataSourceRepository.findByAiId(aiId);
  }

  public async findDataSourcesToMigrate(): Promise<string[]> {
    const dataSourceKnowledges = await prismadb.dataSourceKnowledge.findMany({
      select: {
        dataSourceId: true,
      },
      distinct: ["dataSourceId"],
      where: {
        knowledge: {
          isMigrated: false,
          indexStatus: {
            not: KnowledgeIndexStatus.DELETED,
          },
        },
      },
    });

    return dataSourceKnowledges.map((dk) => dk.dataSourceId);
  }

  public async getOriginalDataSourceIdForKnowledge(
    knowledgeId: string
  ): Promise<string | null> {
    return await this.dataSourceRepository.getOriginalDataSourceIdForKnowledge(
      knowledgeId
    );
  }
}

const aiRepository = new AIRepositoryImpl();
const dataSourceRepository = new DataSourceRepositoryImpl();
const dataSourceViewingService = new DataSourceViewingService(
  aiRepository,
  dataSourceRepository
);
export default dataSourceViewingService;
