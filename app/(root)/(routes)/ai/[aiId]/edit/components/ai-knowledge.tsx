import { useState } from "react";
import DataSourceCard from "./datasource-card";
import {
  PlusCircle,
  FileUp,
  Globe,
  Server,
  Database,
  Network,
  ChevronLeft,
} from "lucide-react";
import { FormField } from "@/components/ui/form";
import { useToast } from "@/components/ui/use-toast";
import { GoogleDriveForm } from "./google-drive-knowledge";
import { useRouter } from "next/navigation";
import { FileUploadKnowledge } from "./file-upload-knowledge";
import { Prisma } from "@prisma/client";

const extendedCompanion = Prisma.validator<Prisma.CompanionDefaultArgs>()({
  include: {
    knowledge: {
      include: {
        knowledge: true,
      },
    },
  },
});

type ExtendedCompanion = Prisma.CompanionGetPayload<typeof extendedCompanion>;
interface SelectDataSourceProps {
  aiId?: string;
  form: any;
  initialAi: ExtendedCompanion | null;
}

export const AIKnowledge = ({
  aiId,
  form,
  initialAi,
}: SelectDataSourceProps) => {
  const { toast } = useToast();
  const router = useRouter();
  const [activeTab, setActiveTab] = useState(0);

  return (
    <div className="h-full p-4 max-w-3xl mx-auto">
      {activeTab === 0 && (
        <>
          <h1 className="text-lg font-medium">Your AI&apos;s Data Sources</h1>
          <p className="text-sm text-muted-foreground">
            The following files and sources are currently being used to inform
            your AI&apos;s knowledge.
          </p>
          <FormField
            name="knowledge"
            control={form.control}
            render={({ field }) => (
              <div>
                {field.value.map((item: any) => (
                  <div
                    key={item.knowledgeId}
                    className="flex items-center justify-between my-2"
                  >
                    <p className="text-sm px-3 py-2 bg-background rounded-lg  w-full ">
                      {item.knowledge.name}
                    </p>
                  </div>
                ))}
                {field.value.length === 0 ? (
                  <div className="flex items-center justify-between my-2">
                    <p className="text-sm px-3 py-2 bg-background rounded-lg w-full ">
                      None
                    </p>
                  </div>
                ) : null}
              </div>
            )}
          />

          <h2 className="text-lg font-medium mt-8">Add more data sources</h2>
          <p className="text-sm text-muted-foreground">
            Choose a data source for your data store
          </p>
          <h3 className="text-md font-medium mt-6 mb-2">Data Sources</h3>
          <div className="grid grid-cols-3 gap-4">
            <DataSourceCard
              icon={PlusCircle}
              title="Your Data Stores"
              description="Select a data store you created for a different AI."
              isSelected={true}
            />
            <DataSourceCard
              icon={FileUp}
              title="Upload Files"
              description="Directly add files as data sources for your AI."
              onClick={() => setActiveTab(1)}
            />
            <DataSourceCard
              icon={Globe}
              title="Website URLs"
              description="Automatically crawl website content from a list of domains you define."
              onClick={() => {
                if (!aiId) {
                  toast({
                    variant: "destructive",
                    description: "Please save your AI first.",
                  });
                } else {
                  router.push(`/ai/${aiId}/knowledge/web-urls`);
                }
              }}
            />
            <DataSourceCard
              icon={Server}
              title="Cloud Storage"
              description="Import data from a cloud storage bucket."
              onClick={() => {
                if (!aiId) {
                  toast({
                    variant: "destructive",
                    description: "Please save your AI first.",
                  });
                } else {
                  setActiveTab(3);
                }
              }}
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
        </>
      )}
      {activeTab !== 0 && (
        <div className="flex items-center mb-8">
          <ChevronLeft
            className="text-ring h-8 w-8 cursor-pointer"
            onClick={() => setActiveTab(0)}
          />
          <h1 className="text-lg font-medium">Create Data Store</h1>
        </div>
      )}
      {activeTab === 1 && (
        <FileUploadKnowledge
          goBack={() => setActiveTab(0)}
          form={form}
          initialAi={initialAi}
        />
      )}
      {activeTab === 2 && <div></div>}
      {activeTab === 3 && aiId && (
        <GoogleDriveForm aiId={aiId} goBack={() => setActiveTab(0)} />
      )}
    </div>
  );
};
