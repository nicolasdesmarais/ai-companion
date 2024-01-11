import { Table } from "@/components/table";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import { DataSourceType, KnowledgeIndexStatus } from "@prisma/client";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import {
  ChevronLeft,
  Coffee,
  FileUp,
  Globe,
  Loader,
  MinusCircle,
  RefreshCcw,
  Server,
} from "lucide-react";
import { usePathname, useRouter } from "next/navigation";
import { useState } from "react";
import DataSourceCard from "./datasource-card";
import { getDataSourceRefreshPeriodLabel } from "./datasource-refresh-periods";
import { DataSourceTypes } from "./datasource-types";
import { FileUploadKnowledge } from "./file-upload-knowledge";
import { GoogleDriveForm } from "./google-drive-knowledge";
import { Banner } from "./ui/banner";
import { WebUrlsForm } from "./web-urls-knowledge-form";

interface SelectDataSourceProps {
  form: any;
  dataSources: any;
  setDataSource: (dataSource: any) => void;
  knowledgeLoading: boolean;
}

const dataSourceTypesForRefresh = [
  DataSourceType.GOOGLE_DRIVE,
  DataSourceType.WEB_URL,
];

export const AIKnowledge = ({
  form,
  dataSources,
  setDataSource,
  knowledgeLoading,
}: SelectDataSourceProps) => {
  const { toast } = useToast();
  const [removing, setRemoving] = useState("");
  const [refreshing, setRefreshing] = useState("");
  const pathname = usePathname();
  const router = useRouter();
  const aiId = form.getValues("id");

  const removeDataSource = async (id: string) => {
    setRemoving(id);
    try {
      await axios.delete(`/api/v1/data-sources/${id}/`);

      setDataSource((current: any) => current.filter((i: any) => i.id !== id));
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

  const refreshDataSource = async (id: string) => {
    setRefreshing(id);
    try {
      await axios.put(`/api/v1/data-sources/${id}/refresh`);

      toast({ description: "Data source refresh request accepted." });
    } catch (error: any) {
      toast({
        variant: "destructive",
        description:
          String((error as AxiosError).response?.data) ||
          "Something went wrong.",
        duration: 6000,
      });
    }
    setRefreshing("");
  };

  const inProgress = dataSources.some(
    (dataSource: any) =>
      dataSource.indexPercentage !== "100" &&
      dataSource.indexStatus !== "FAILED"
  );

  return (
    <div className="h-full p-4 max-w-3xl mx-auto">
      {pathname.endsWith("knowledge") && (
        <>
          <h1 className="text-lg font-medium">Your AI&apos;s Data Sources</h1>
          <p className="text-sm text-muted-foreground">
            The following files and sources are currently being used to inform
            your AI&apos;s knowledge.
          </p>
          {inProgress && (
            <Banner className="my-2">
              Data sources are being indexed. You can continue without loosing
              any progress.
              <div>Return later to check on progress.</div>
            </Banner>
          )}
          <div className="max-h-96 overflow-auto">
            <Table
              headers={[
                "NAME",
                "TYPE",
                "Refresh Period",
                "LAST MODIFIED",
                "Progress",
                "Remove",
              ]}
              className="w-full my-4 max-h-60"
            >
              {dataSources.map((dataSource: any) => (
                <tr key={dataSource.id} className="items-center my-2 text-sm">
                  <td className="p-2 ">
                    <div className="max-w-sm truncate">{dataSource.name}</div>
                  </td>
                  <td className="p-2">
                    {
                      DataSourceTypes.find(
                        (format) => format.type === dataSource.type
                      )?.name
                    }
                  </td>
                  <td className="p-3 flex justify-between items-center">
                    {getDataSourceRefreshPeriodLabel(dataSource.refreshPeriod)}
                    {dataSourceTypesForRefresh.includes(dataSource.type) && (
                      <Button
                        type="button"
                        variant="outline"
                        size="sm"
                        disabled={!!removing}
                        onClick={() => refreshDataSource(dataSource.id)}
                      >
                        {refreshing === dataSource.id ? (
                          <Loader className="w-4 h-4 spinner" />
                        ) : (
                          <RefreshCcw className="w-4 h-4" />
                        )}
                      </Button>
                    )}
                  </td>
                  <td className="p-2">
                    {dataSource.lastIndexedAt
                      ? format(
                          new Date(dataSource.lastIndexedAt),
                          "h:mma M/d/yyyy "
                        )
                      : null}
                  </td>
                  <td className="p-2">
                    {dataSource.indexStatus === KnowledgeIndexStatus.FAILED
                      ? "Failed"
                      : Math.round(dataSource.indexPercentage) + "%"}
                  </td>
                  <td className="p-2 text-center">
                    <Button
                      type="button"
                      variant="outline"
                      disabled={!!removing}
                      onClick={() => removeDataSource(dataSource.id)}
                    >
                      {removing === dataSource.id ? (
                        <Loader className="w-4 h-4 spinner" />
                      ) : (
                        <MinusCircle className="w-4 h-4 text-destructive" />
                      )}
                    </Button>
                  </td>
                </tr>
              ))}
            </Table>
            {!knowledgeLoading && dataSources.length === 0 ? (
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
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {/* <DataSourceCard
              icon={PlusCircle}
              title="Your Data Stores"
              description="Select a data store you created for a different AI."
              isDisabled={true}
            /> */}
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
            {/* <DataSourceCard
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
            /> */}
          </div>
        </>
      )}
      {!pathname.endsWith("knowledge") && (
        <div className="flex items-center mb-8">
          <ChevronLeft
            className="text-ring h-8 w-8 cursor-pointer"
            onClick={() => router.push(`/ai/${aiId}/edit/knowledge`)}
          />
          <h1
            className="text-lg font-medium cursor-pointer"
            onClick={() => router.push(`/ai/${aiId}/edit/knowledge`)}
          >
            Create Data Source
          </h1>
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
