import { DataSourcesSearch } from "./datasources-search";
import { DataSourcesTable } from "./datasources-table";

export const DataSources = () => {
  return (
    <div className="mt-2">
      <DataSourcesSearch />
      <DataSourcesTable />
    </div>
  );
};
