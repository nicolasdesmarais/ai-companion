import { Table } from "@/components/table";
import {
  FormControl,
  FormDescription,
  FormItem,
  FormMessage,
} from "@/components/ui/form";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { useToast } from "@/components/ui/use-toast";
import { AIModel } from "@/src/domain/models/AIModel";
import { cn } from "@/src/lib/utils";
import { DataSourceIndexStatus, DataSourceType } from "@prisma/client";
import axios from "axios";
import { format } from "date-fns";
import {
  ChevronLeft,
  Coffee,
  FileUp,
  Globe,
  Loader,
  PlusCircle,
  ShieldCheck,
} from "lucide-react";
import { usePathname, useRouter } from "next/navigation";
import { useEffect, useState } from "react";
import { ConfirmModal } from "./confirm-modal";
import { ConnectKnowledge } from "./connect-knowledge";
import DataSourceCard from "./datasource-card";
import { DataSourceDetailModal } from "./datasource-detail-modal";
import { getDataSourceRefreshPeriodLabel } from "./datasource-refresh-periods";
import { DataSourceTypes } from "./datasource-types";
import { FileUploadKnowledge } from "./file-upload-knowledge";
import { GoogleDriveForm } from "./google-drive-knowledge";
import { OneDriveKnowledge } from "./onedrive-knowledge";
import { GoogleDriveSvg } from "./svg/google-drive-svg";
import { OneDriveSvg } from "./svg/onedrive-svg";
import { Banner } from "./ui/banner";
import { Tooltip } from "./ui/tooltip";
import { WebUrlsForm } from "./web-urls-knowledge-form";

const needsRefresh = (status: DataSourceIndexStatus) =>
  status !== DataSourceIndexStatus.COMPLETED &&
  status !== DataSourceIndexStatus.PARTIALLY_COMPLETED &&
  status !== DataSourceIndexStatus.FAILED &&
  status !== DataSourceIndexStatus.DELETED &&
  status !== DataSourceIndexStatus.MISSING;

interface SelectDataSourceProps {
  form: any;
  dataSources: any;
  setDataSource: (dataSource: any) => void;
  knowledgeLoading: boolean;
  aiModels: AIModel[];
  fetchDataSources: () => void;
}

const dataSourceTypesForRefresh = [
  DataSourceType.GOOGLE_DRIVE,
  DataSourceType.WEB_URL,
  DataSourceType.ONEDRIVE,
];

export const AIKnowledge = ({
  form,
  dataSources,
  setDataSource,
  knowledgeLoading,
  aiModels,
  fetchDataSources,
}: SelectDataSourceProps) => {
  const { toast } = useToast();
  const [modelId, setModelId] = useState(form.getValues("modelId"));
  const [detailDataSource, setDetailDataSource] = useState<any>(null);
  const pathname = usePathname();
  const router = useRouter();
  const aiId = form.getValues("id");

  const autoRefresh = dataSources.some((dataSource: any) =>
    needsRefresh(dataSource.indexStatus)
  );
  const isLoading = form.formState.isSubmitting;

  const saveModel = async (id: string) => {
    try {
      const values = form.getValues();
      values.modelId = id;
      setModelId(id);
      await axios.patch(`/api/v1/ai/${aiId}`, values);
      toast({
        description: "AI Saved.",
        duration: 2000,
      });
    } catch (error) {
      toast({
        variant: "destructive",
        description: "Something went wrong.",
        duration: 3000,
      });
    }
  };

  useEffect(() => {
    const intervalId = setInterval(() => {
      if (autoRefresh && !isLoading && !detailDataSource) {
        fetchDataSources();
      }
    }, 5000);

    return () => clearInterval(intervalId);
  }, [autoRefresh]);

  const inProgress = dataSources.some(
    (dataSource: any) =>
      dataSource.indexPercentage !== "100" &&
      dataSource.indexStatus !== "FAILED"
  );

  return (
    <div className="h-full p-4 max-w-3xl mx-auto">
      {pathname.endsWith("knowledge") && (
        <>
          <h1 className="text-lg font-medium mb-2">Your AI&apos;s Model</h1>
          <FormItem>
            <Select
              disabled={isLoading}
              onValueChange={(val) => saveModel(val)}
              value={modelId}
            >
              <FormControl>
                <SelectTrigger className="bg-background w-1/2">
                  <SelectValue placeholder="Select a model" />
                </SelectTrigger>
              </FormControl>
              <SelectContent>
                {aiModels.map((model) => (
                  <SelectItem key={model.id} value={model.id}>
                    <div className="flex items-center">
                      {model.name}
                      {model.isPrivate ? (
                        <ShieldCheck className="w-4 h-4 text-green ml-2" />
                      ) : null}
                    </div>
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
            <FormDescription>
              Select the Large Language Model for your AI. Models marked with a
              <ShieldCheck className="w-3 h-3 text-green inline ml-1" /> are
              hosted on a private instance.
            </FormDescription>
            <FormMessage />
          </FormItem>
          <h1 className="text-lg font-medium mt-8">
            Your AI&apos;s Data Sources
          </h1>
          <p className="text-sm text-muted-foreground">
            The following files and sources are currently being used to inform
            your AI&apos;s knowledge.
          </p>
          {inProgress && (
            <Banner className="my-2">
              Data sources are being indexed. You can continue without losing
              any progress.
              <div>Return later to check on progress.</div>
            </Banner>
          )}
          <div className="max-h-96 overflow-y-auto overflow-x-hidden">
            <Table
              headers={[
                "Name",
                "Type",
                "Refresh Period",
                "Last Modified",
                "Progress",
              ]}
              className="w-full my-4 max-h-60"
            >
              {dataSources.map((dataSource: any) => (
                <tr
                  key={dataSource.id}
                  onClick={() => {
                    if (dataSource.knowledges.length || dataSource.data.error) {
                      setDetailDataSource(dataSource);
                    }
                  }}
                  className={cn(
                    "items-center my-2 text-sm",
                    (dataSource.knowledges.length > 0 ||
                      dataSource.data.error) &&
                      "cursor-pointer hover:bg-primary/10"
                  )}
                >
                  <td className={cn("p-2")}>
                    {dataSource.name.length > 30 ? (
                      <Tooltip content={dataSource.name} className="">
                        <div className="truncate max-w-[280px]">
                          {dataSource.name}
                        </div>
                      </Tooltip>
                    ) : (
                      <div className="truncate max-w-[280px]">
                        {dataSource.name}
                      </div>
                    )}
                  </td>
                  <td className="p-2">
                    {
                      DataSourceTypes.find(
                        (format) => format.type === dataSource.type
                      )?.name
                    }
                  </td>
                  {dataSourceTypesForRefresh.includes(dataSource.type) &&
                  dataSource.indexStatus !== DataSourceIndexStatus.MISSING ? (
                    <td className="p-3 flex justify-between items-center">
                      {getDataSourceRefreshPeriodLabel(
                        dataSource.refreshPeriod
                      )}
                    </td>
                  ) : (
                    <td className="p-2">
                      {getDataSourceRefreshPeriodLabel(
                        dataSource.refreshPeriod
                      )}
                    </td>
                  )}
                  <td className="p-2">
                    {dataSource.lastIndexedAt
                      ? format(
                          new Date(dataSource.lastIndexedAt),
                          "h:mma M/d/yyyy "
                        )
                      : null}
                  </td>
                  <td className="p-2">
                    <div className="flex items-center">
                      {dataSource.indexStatus === DataSourceIndexStatus.FAILED
                        ? "Failed"
                        : dataSource.indexStatus ===
                          DataSourceIndexStatus.MISSING
                        ? "Missing"
                        : Math.round(dataSource.indexPercentage) + "%"}
                      {needsRefresh(dataSource.indexStatus) && (
                        <Loader className="w-4 h-4 spinner ml-1" />
                      )}
                    </div>
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
            {knowledgeLoading && dataSources.length === 0 ? (
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
            <DataSourceCard
              icon={PlusCircle}
              title="Your Data Stores"
              description="Select a data store you created for a different AI."
              onClick={() => router.push(`/ai/${aiId}/edit/knowledge/connect`)}
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
              icon={GoogleDriveSvg}
              title="Google Drive"
              description="Import data from your Google cloud storage."
              onClick={() =>
                router.push(`/ai/${aiId}/edit/knowledge/google-drive`)
              }
            />
            <DataSourceCard
              icon={OneDriveSvg}
              title="Microsoft OneDrive"
              description="Import data from your Microsoft cloud storage."
              onClick={() =>
                router.push(`/ai/${aiId}/edit/knowledge/one-drive`)
              }
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
          <h1
            className="text-lg font-medium cursor-pointer"
            onClick={() => router.push(`/ai/${aiId}/edit/knowledge`)}
          >
            Create Data Source
          </h1>
        </div>
      )}
      {pathname.endsWith("connect") && (
        <ConnectKnowledge
          goBack={() => router.push(`/ai/${aiId}/edit/knowledge`)}
          form={form}
        />
      )}
      {pathname.endsWith("file") && (
        <FileUploadKnowledge
          goBack={() => router.push(`/ai/${aiId}/edit/knowledge`)}
          form={form}
        />
      )}
      {pathname.endsWith("web-url") && <WebUrlsForm aiId={aiId} />}
      {pathname.endsWith("google-drive") && aiId && (
        <GoogleDriveForm
          aiId={aiId}
          goBack={() => router.push(`/ai/${aiId}/edit/knowledge`)}
        />
      )}
      {pathname.endsWith("one-drive") && aiId && (
        <OneDriveKnowledge
          aiId={aiId}
          goBack={() => router.push(`/ai/${aiId}/edit/knowledge`)}
        />
      )}
      <ConfirmModal />
      {detailDataSource && (
        <DataSourceDetailModal
          dataSource={detailDataSource}
          onClose={() => setDetailDataSource(null)}
          toast={toast}
          setDataSource={setDataSource}
          aiId={aiId}
          fetchDataSources={fetchDataSources}
        />
      )}
    </div>
  );
};
