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
  Coffee,
} from "lucide-react";
import { useToast } from "@/components/ui/use-toast";
import { GoogleDriveForm } from "./google-drive-knowledge";
import { FileUploadKnowledge } from "./file-upload-knowledge";
import { WebUrlsForm } from "./web-urls-knowledge-form";
import axios, { AxiosError } from "axios";
import { Button } from "@/components/ui/button";
import { useRouter, usePathname } from "next/navigation";
import Link from "next/link";
import { Table } from "@/components/table";
import { knowledgeTypes } from "@/components/knowledge-types";
import { format } from "date-fns";
interface SelectDataSourceProps {
  form: any;
  knowledge: any;
  setKnowledge: (knowledge: any) => void;
  knowledgeLoading: boolean;
}

export const AIKnowledge = ({
  form,
  knowledge,
  setKnowledge,
  knowledgeLoading,
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

          <div className="max-h-96 overflow-auto">
            <Table
              headers={["NAME", "TYPE", "LAST MODIFIED", "Progress", "Remove"]}
              className="w-full my-4 max-h-60"
            >
              {knowledge.map((knowledge: any) => (
                <tr key={knowledge.id} className="items-center my-2 text-sm">
                  <td className="p-2 ">
                    {knowledge.blobUrl ? (
                      <Link href={knowledge.blobUrl}>
                        <div className="text-ring max-w-sm truncate">
                          {knowledge.name}
                        </div>
                      </Link>
                    ) : (
                      <div className="max-w-sm truncate">{knowledge.name}</div>
                    )}
                  </td>
                  <td className="p-2">
                    {
                      knowledgeTypes.find(
                        (format) => format.type === knowledge.type
                      )?.name
                    }
                  </td>
                  <td className="p-2">
                    {format(new Date(knowledge.updatedAt), "h:mma M/d/yyyy ")}
                  </td>
                  <td className="p-2 text-center">
                    {knowledge.blobUrl ? "100%" : ""}
                  </td>
                  <td className="p-2 text-center">
                    <Button
                      type="button"
                      variant="outline"
                      disabled={!!removing}
                      onClick={() => removeKnowledge(knowledge.id)}
                    >
                      {removing === knowledge.id ? (
                        <Loader className="w-4 h-4 spinner" />
                      ) : (
                        <MinusCircle className="w-4 h-4 text-destructive" />
                      )}
                    </Button>
                  </td>
                </tr>
              ))}
            </Table>
            {!knowledgeLoading && knowledge.length === 0 ? (
              <div className="flex items-center my-2 w-full">
                <div className="mx-auto flex p-4 bg-background rounded-lg">
                  <Coffee className="w-6 h-6 mr-2" />
                  <p>Load data to see it here</p>
                </div>
              </div>
            ) : null}
            {knowledgeLoading ? (
              <div className="flex items-center my-2 w-full">
                <div className="mx-auto">
                  <Loader className="w-8 h-8 spinner" />
                </div>
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
