import { DataStores } from "@/components/data-stores";

export default async function DataSourcesPage() {
  return (
    <div className="pl-2 pr-4">
      <h1 className="text-3xl font-bold whitespace-nowrap pt-2 pr-2">
        Data Sources
      </h1>
      <DataStores />
    </div>
  );
}
