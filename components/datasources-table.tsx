"use client";
import { Table } from "@/components/table";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { useToast } from "@/components/ui/use-toast";
import { cn } from "@/src/lib/utils";
import { KnowledgeIndexStatus } from "@prisma/client";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";
import { useEffect, useState } from "react";
import { DataSourceTypes } from "./datasource-types";
import { DataSourcesDetails } from "./datasources-detail";
import { Tooltip } from "./ui/tooltip";

export const DataSourcesTable = () => {
  const [dataSources, setDataSources] = useState<any[]>([]);

  const router = useRouter();
  const searchParams = useSearchParams();

  const search = searchParams.get("search");
  const orderBy = searchParams.get("orderBy");
  const focus = searchParams.get("focus");

  const { toast } = useToast();

  const fetchDataSources = async () => {
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
        <Table
          headers={[
            "Name",
            "AI",
            "Type",
            "Last Modified",
            "Progress",
            "Status",
          ]}
          className="grow my-4 max-h-60"
        >
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
                      <div className="max-w-[280px] truncate">
                        {dataSource.name}
                      </div>
                    </Tooltip>
                  ) : (
                    <div className="truncate max-w-[280px]">
                      {dataSource.name}
                    </div>
                  )}
                </td>
                <td className="p-2 flex flex-wrap max-w-[145px] min-w-[115px]">
                  {dataSource.ais.map((ai: any) => (
                    <div key={`ai-${ai.id}`}>
                      <Tooltip content={ai.ai.name} className="cursor-default">
                        <Avatar className="h-6 w-6 mr-2">
                          <AvatarImage src={ai.ai.src} crop="w_48,h_48" />
                        </Avatar>
                      </Tooltip>
                    </div>
                  ))}
                </td>
                <td className="p-2">
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
                <td className="p-2">{dataSource.indexStatus}</td>
              </tr>
            </>
          ))}
        </Table>
        <DataSourcesDetails
          dataSources={dataSources}
          onChange={fetchDataSources}
        />
      </div>
    </div>
  );
};
