"use client";
import { useEffect } from "react";
import { Table } from "@/components/table";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { ChevronRight, ChevronDown, Loader, MinusCircle } from "lucide-react";
import { useState } from "react";
import { DataSourceTypes } from "./datasource-types";
import { KnowledgeIndexStatus } from "@prisma/client";
import Link from "next/link";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";
import { cn } from "@/src/lib/utils";

export const DataStoresTable = () => {
  const [dataSources, setDataSources] = useState<any[]>([]);
  const [removing, setRemoving] = useState("");

  const router = useRouter();
  const searchParams = useSearchParams();

  const focus = searchParams.get("focus");

  const { toast } = useToast();

  const removeDataSource = async (id: string) => {};

  const fetchDataSources = async () => {
    try {
      const response = await axios.get(`/api/v1/super/data-sources/`);
      setDataSources(response.data);
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
  }, []);

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
      <Table
        headers={[
          "NAME",
          "AIs",
          "TYPE",
          "LAST MODIFIED",
          "Progress",
          "Status",
          "Remove",
        ]}
        className="w-full my-4 max-h-60"
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
              <td className="p-2 ">
                <div className=" truncate text-ring cursor-pointer flex items-center">
                  {dataSource.name}
                </div>
              </td>
              <td className="p-2 max-w-sm truncate">
                {dataSource.ais.map((ai: any) => {
                  return (
                    <Link
                      key={ai.ai.id}
                      target="_blank"
                      href={`/ai/${ai.ai.id}`}
                      className="text-ring"
                    >
                      {ai.ai.name}
                    </Link>
                  );
                })}
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
          </>
        ))}
      </Table>
    </div>
  );
};
