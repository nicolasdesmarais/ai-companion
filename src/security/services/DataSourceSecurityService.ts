import { DataSourceDto } from "@/src/domain/models/DataSources";
import { DataSource } from "@prisma/client";
import { AuthorizationContext } from "../models/AuthorizationContext";
import { SecuredAction } from "../models/SecuredAction";
import { SecuredResourceType } from "../models/SecuredResourceType";
import {
  BaseEntity,
  BaseEntitySecurityService,
} from "./BaseEntitySecurityService";

export class DataSourceSecurityService {
  public static canReadDataSource(
    authorizationContext: AuthorizationContext,
    dataSource: DataSourceDto | DataSource
  ) {
    return BaseEntitySecurityService.hasPermissionOnEntity(
      authorizationContext,
      this.mapToBaseEntity(dataSource),
      SecuredResourceType.DATA_SOURCES,
      SecuredAction.READ
    );
  }

  public static canUpdateDataSource(
    authorizationContext: AuthorizationContext,
    dataSource: DataSourceDto | DataSource
  ) {
    return BaseEntitySecurityService.hasPermissionOnEntity(
      authorizationContext,
      this.mapToBaseEntity(dataSource),
      SecuredResourceType.DATA_SOURCES,
      SecuredAction.WRITE
    );
  }

  private static mapToBaseEntity(
    dataSource: DataSourceDto | DataSource
  ): BaseEntity {
    return { orgId: dataSource.orgId, userId: dataSource.ownerUserId };
  }
}
