import { DataSources } from "@/components/datasources";
import { PaywallBanner } from "@/components/paywall-banner";

export default async function DataSourcesPage() {
  return (
    <div className="pl-2 pr-4">
      <PaywallBanner className="mt-3" />
      <h1 className="text-3xl font-bold whitespace-nowrap pt-2 pr-2">
        Data Sources
      </h1>
      <DataSources />
    </div>
  );
}
