import { DataSourceRefreshPeriod } from "@prisma/client";

const dataRefreshPeriodLabels = {
  [DataSourceRefreshPeriod.NEVER]: "Never",
  [DataSourceRefreshPeriod.DAILY]: "Daily",
  [DataSourceRefreshPeriod.WEEKLY]: "Weekly",
  [DataSourceRefreshPeriod.MONTHLY]: "Monthly",
};

export function getDataSourceRefreshPeriodLabel(
  refreshPeriod: DataSourceRefreshPeriod | null
) {
  return dataRefreshPeriodLabels[
    refreshPeriod ? refreshPeriod : DataSourceRefreshPeriod.NEVER
  ];
}
