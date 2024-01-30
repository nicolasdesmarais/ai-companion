import { DataStoresSearch } from "./data-stores-search";
import { DataStoresTable } from "./data-stores-table";

export const DataStores = () => {
  return (
    <div className="mt-2">
      <DataStoresSearch />
      <DataStoresTable />
    </div>
  );
};
