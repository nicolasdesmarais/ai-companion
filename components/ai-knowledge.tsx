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
  Loader,
  MinusCircle,
} from "lucide-react";
import { useToast } from "@/components/ui/use-toast";
import { GoogleDriveForm } from "./google-drive-knowledge";
import { FileUploadKnowledge } from "./file-upload-knowledge";
import { WebUrlsForm } from "./web-urls-knowledge-form";
import axios, { AxiosError } from "axios";
import { Button } from "@/components/ui/button";
import { useRouter, usePathname } from "next/navigation";
import Link from "next/link";
interface SelectDataSourceProps {
  form: any;
  knowledge: any;
  setKnowledge: (knowledge: any) => void;
}

export const AIKnowledge = ({
  form,
  knowledge,
  setKnowledge,
}: SelectDataSourceProps) => {
  const { toast } = useToast();
  const [removing, setRemoving] = useState("");
  const pathname = usePathname();
  const router = useRouter();
  const aiId = form.getValues("id");

  const removeKnowledge = async (id: string) => {
    setRemoving(id);
    try {
      await axios.delete(`/api/knowledge/${id}/${aiId}`);

      setKnowledge((current: any) => current.filter((i: any) => i.id !== id));
      toast({ description: "Knowledge removed." });
    } catch (error: any) {
      toast({
        variant: "destructive",
        description:
          String((error as AxiosError).response?.data) ||
          "Something went wrong.",
        duration: 6000,
      });
    }
    setRemoving("");
  };

  return (
    <div className="h-full p-4 max-w-3xl mx-auto">
      {pathname.endsWith("knowledge") && (
        <>
          <h1 className="text-lg font-medium">Your AI&apos;s Data Sources</h1>
          <p className="text-sm text-muted-foreground">
            The following files and sources are currently being used to inform
            your AI&apos;s knowledge.
          </p>

          <div>
            {knowledge.map((knowledge: any) => (
              <div
                key={knowledge.id}
                className="flex items-center justify-between my-2"
              >
                <p className="text-sm px-3 py-2 bg-background rounded-lg w-full text-ellipsis">
                  {knowledge.blobUrl ? (
                    <Link href={knowledge.blobUrl} className="text-ring">
                      {knowledge.name}
                    </Link>
                  ) : (
                    knowledge.name
                  )}
                </p>
                <Button
                  type="button"
                  variant="outline"
                  disabled={!!removing}
                  onClick={() => removeKnowledge(knowledge.id)}
                >
                  {removing === knowledge.id ? (
                    <Loader className="w-4 h-4 spinner" />
                  ) : (
                    <MinusCircle className="w-4 h-4" />
                  )}
                </Button>
              </div>
            ))}
            {knowledge.length === 0 ? (
              <div className="flex items-center justify-between my-2">
                <p className="text-sm px-3 py-2 bg-background rounded-lg w-full ">
                  None
                </p>
              </div>
            ) : null}
          </div>

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
              isDisabled={true}
            />
            <DataSourceCard
              icon={FileUp}
              title="Upload Files"
              description="Directly add files as data sources for your AI."
              onClick={() => router.push(`/ai/${aiId}/edit/knowledge/file`)}
            />
            <DataSourceCard
              icon={Globe}
              title="Website URLs"
              description="Automatically crawl website content from a list of domains you define."
              onClick={() => router.push(`/ai/${aiId}/edit/knowledge/web-url`)}
            />
            <DataSourceCard
              icon={Server}
              title="Cloud Storage"
              description="Import data from a cloud storage bucket."
              onClick={() => router.push(`/ai/${aiId}/edit/knowledge/cloud`)}
            />
            <DataSourceCard
              icon={Database}
              title="SQL Query"
              description="Import data from a SQL table."
              isDisabled={true}
            />
            <DataSourceCard
              icon={Network}
              title="API"
              description="Import data manually by calling an API."
              isDisabled={true}
            />
          </div>
        </>
      )}
      {!pathname.endsWith("knowledge") && (
        <div className="flex items-center mb-8">
          <ChevronLeft
            className="text-ring h-8 w-8 cursor-pointer"
            onClick={() => router.push(`/ai/${aiId}/edit/knowledge`)}
          />
          <h1 className="text-lg font-medium">Create Data Source</h1>
        </div>
      )}
      {pathname.endsWith("file") && (
        <FileUploadKnowledge
          goBack={() => router.push(`/ai/${aiId}/edit/knowledge`)}
          form={form}
        />
      )}
      {pathname.endsWith("web-url") && <WebUrlsForm aiId={aiId} />}
      {pathname.endsWith("cloud") && aiId && (
        <GoogleDriveForm
          aiId={aiId}
          goBack={() => router.push(`/ai/${aiId}/edit/knowledge`)}
        />
      )}
    </div>
  );
};
