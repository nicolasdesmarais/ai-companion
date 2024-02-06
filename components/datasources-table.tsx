"use client";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { useToast } from "@/components/ui/use-toast";
import { cn } from "@/src/lib/utils";
import { KnowledgeIndexStatus } from "@prisma/client";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { Loader } from "lucide-react";
import Image from "next/image";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";
import { useEffect, useState } from "react";
import { DataSourceTypes } from "./datasource-types";
import { DataSourcesDetails } from "./datasources-detail";
import { Tooltip } from "./ui/tooltip";

export const DataSourcesTable = () => {
  const [dataSources, setDataSources] = useState<any[]>([]);
  const [loading, setLoading] = useState<boolean>(true);

  const router = useRouter();
  const searchParams = useSearchParams();

  const search = searchParams.get("search");
  const orderBy = searchParams.get("orderBy");
  const focus = searchParams.get("focus");

  const { toast } = useToast();

  const fetchDataSources = async () => {
    setLoading(true);
    try {
      const url = qs.stringifyUrl(
        {
          url: "/api/v1/data-sources",
          query: {
            search: search ? `*${search}*` : null,
            orderBy: orderBy ? `+${orderBy}` : null,
          },
        },
        { skipNull: true, skipEmptyString: true }
      );
      const response = await axios.get(url);
      setDataSources(response.data.data);
      setLoading(false);
    } catch (error: any) {
      toast({
        variant: "destructive",
        description:
          String((error as AxiosError).response?.data) ||
          "Something went wrong.",
        duration: 6000,
      });
    }
  };

  useEffect(() => {
    fetchDataSources();
  }, [search, orderBy]);

  const select = (id: string) => {
    const query = {
      focus: id,
    };

    const url = qs.stringifyUrl(
      {
        url: window.location.href,
        query,
      },
      { skipNull: true, skipEmptyString: true }
    );

    router.push(url);
  };

  return (
    <div className="mt-2">
      <div className="flex w-full">
        <div className="flex flex-col w-full">
          <table className="table-auto text-left grow my-4 max-h-60">
            <thead className="border-y-2 p-2 border-ring">
              <tr>
                <th className="p-2 text-sm">Name</th>
                <th className="p-2 text-sm">AI</th>
                <th className="p-2 text-sm hidden md:block">Type</th>
                <th className="p-2 text-sm">Last Modified</th>
                <th className="p-2 text-sm">Progress</th>
                <th className="p-2 text-sm hidden md:block">Status</th>
              </tr>
            </thead>
            <tbody className="text-sm">
              {dataSources.map((dataSource: any) => (
                <>
                  <tr
                    key={dataSource.id}
                    className={cn(
                      "items-center my-2 text-sm hover:bg-ring/10",
                      focus === dataSource.id && "bg-ring/10"
                    )}
                    onClick={() => select(dataSource.id)}
                  >
                    <td className="p-2">
                      {dataSource.name.length > 30 ? (
                        <Tooltip
                          content={dataSource.name}
                          className="cursor-default"
                        >
                          <div className="max-w-[100px] md:max-w-[280px] truncate">
                            {dataSource.name}
                          </div>
                        </Tooltip>
                      ) : (
                        <div className="truncate max-w-[100px] md:max-w-[280px]">
                          {dataSource.name}
                        </div>
                      )}
                    </td>
                    <td className="p-2 flex flex-wrap max-w-[145px] min-w-[115px]">
                      {dataSource.ais.map((ai: any) => (
                        <div key={`ai-${ai.id}`}>
                          <Tooltip
                            content={ai.ai.name}
                            className="cursor-default"
                          >
                            <Avatar className="h-6 w-6 mr-2">
                              <AvatarImage src={ai.ai.src} crop="w_48,h_48" />
                            </Avatar>
                          </Tooltip>
                        </div>
                      ))}
                    </td>
                    <td className="p-2 hidden md:table-cell">
                      {
                        DataSourceTypes.find(
                          (format) => format.type === dataSource.type
                        )?.name
                      }
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
                    <td className="p-2 hidden md:table-cell">
                      {dataSource.indexStatus}
                    </td>
                  </tr>
                </>
              ))}
            </tbody>
          </table>
          {dataSources.length === 0 && loading && (
            <div className="flex justify-center items-center h-32 w-full">
              <Loader className="w-16 h-16 spinner" />
            </div>
          )}
          {dataSources.length === 0 && !loading && (
            <div className="pt-10 flex flex-col items-center justify-center space-y-3">
              <div className="relative w-60 h-60">
                <Image
                  fill
                  className="grayscale"
                  src="/empty.png"
                  alt="Empty"
                />
              </div>
              <p className="text-sm text-muted-foreground">
                No Data Sources found.
              </p>
            </div>
          )}
        </div>
        <DataSourcesDetails
          dataSources={dataSources}
          onChange={fetchDataSources}
        />
      </div>
    </div>
  );
};
