import { DataSourceIndexStatus, DataSourceType } from "@prisma/client";

export interface DataSourceDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  lastIndexedAt: Date | null;
  orgId: string;
  ownerUserId: string;
  name: string;
  type: DataSourceType;
  indexStatus: DataSourceIndexStatus | null;
  indexPercentage: string;
}
