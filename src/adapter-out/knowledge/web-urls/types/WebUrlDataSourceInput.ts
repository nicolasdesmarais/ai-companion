import { DataSourceRefreshPeriod } from "@prisma/client";

export interface WebUrlDataSourceInput {
  url: string;
  dataRefreshPeriod?: DataSourceRefreshPeriod;
}
