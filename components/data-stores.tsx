import { DataStoresDetails } from "./data-stores-detail";
import { DataStoresSearch } from "./data-stores-search";
import { DataStoresTable } from "./data-stores-table";

export const DataStores = () => {
  return (
    <div className="mt-2">
      <DataStoresSearch />
      <div className="flex">
        <DataStoresTable />
        <DataStoresDetails />
      </div>
    </div>
  );
};
