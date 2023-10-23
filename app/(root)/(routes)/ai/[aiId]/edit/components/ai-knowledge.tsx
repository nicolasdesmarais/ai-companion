import DataSourceCard from "./datasource-card";
import {
  PlusCircle,
  FileUp,
  Globe,
  Server,
  Database,
  Network,
} from "lucide-react";

interface SelectDataSourceProps {
  aiId?: string;
}

export const AIKnowledge = ({ aiId }: SelectDataSourceProps) => {
  return (
    <div className="h-full p-4 max-w-3xl mx-auto">
      <h1 className="text-lg font-medium">Select a data source</h1>
      <p className="text-sm text-muted-foreground">
        Choose a data source for your data store
      </p>

      <h3 className="text-md font-medium mt-6 mb-2">Data Sources</h3>
      <div className="grid grid-cols-3 gap-4">
        <DataSourceCard
          icon={PlusCircle}
          title="Your Data Stores"
          description="Select a data store you created for a different AI."
          href=""
          isSelected={true}
        />
        <DataSourceCard
          icon={FileUp}
          title="Upload Files"
          description="Directly add files as data sources for your AI."
          href=""
        />
        <DataSourceCard
          icon={Globe}
          title="Website URLs"
          description="Automatically crawl website content from a list of domains you define."
          href={`/ai/${aiId}/knowledge/web-urls`}
        />
        <DataSourceCard
          icon={Server}
          title="Cloud Storage"
          description="Import data from a cloud storage bucket."
          href={`/ai/${aiId}/knowledge/google-drive`}
        />
        {/* <DataSourceCard
          icon={Database}
          title="SQL Query"
          description="Import data from a SQL table."
          href=""
        />
        <DataSourceCard
          icon={Network}
          title="API"
          description="Import data manually by calling an API."
          href=""
        /> */}
      </div>
    </div>
  );
};
