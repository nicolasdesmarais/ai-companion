import { DataSourceRepositoryImpl } from "@/src/adapter-out/repositories/DataSourceRepositoryImpl";
import prismadb from "@/src/lib/prismadb";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { AISecurityService } from "@/src/security/services/AISecurityService";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import { DataSourceDto, DataSourceFilter } from "../models/DataSources";
import { DataSourceRepository } from "../ports/outgoing/DataSourceRepository";

export class DataSourceViewingService {
  constructor(private dataSourceRepository: DataSourceRepository) {}

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
    const ai = await prismadb.aI.findUnique({
      where: { id: aiId },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id=${aiId} not found`);
    }

    const canReadAi = AISecurityService.canReadAI(authorizationContext, ai);
    if (!canReadAi) {
      throw new ForbiddenError(
        "User is not authorized to read AI with id=${aiId}"
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
        },
      },
    });

    return dataSourceKnowledges.map((dk) => dk.dataSourceId);
  }
}

const dataSourceRepository = new DataSourceRepositoryImpl();
const dataSourceViewingService = new DataSourceViewingService(
  dataSourceRepository
);
export default dataSourceViewingService;
