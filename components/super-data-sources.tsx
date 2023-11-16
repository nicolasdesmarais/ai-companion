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

export const SuperDataSources = () => {
  const [dataSources, setDataSources] = useState<any[]>([]);
  const [removing, setRemoving] = useState("");
  const [expanded, setExpanded] = useState<string[]>([]);
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

  const onCopy = (content: string) => {
    if (!content) {
      return;
    }

    navigator.clipboard.writeText(content);
    toast({
      description: "Copied to clipboard.",
      duration: 3000,
    });
  };

  const toggle = (id: string) => {
    setExpanded((expanded) => {
      if (expanded.includes(id)) {
        return expanded.filter((i) => i !== id);
      }

      return [...expanded, id];
    });
  };

  return (
    <div>
      <Table
        headers={[
          "NAME",
          "AIs",
          "TYPE",
          "LAST MODIFIED",
          "Id",
          "Progress",
          "Status",
          "Docs",
          "Tokens",
          "Remove",
        ]}
        className="w-full my-4 max-h-60"
      >
        {dataSources.map((dataSource: any) => (
          <>
            <tr key={dataSource.id} className="items-center my-2 text-sm">
              <td className="p-2 ">
                <div
                  onClick={() => toggle(dataSource.id)}
                  className=" truncate text-ring cursor-pointer flex items-center"
                >
                  {expanded.includes(dataSource.id) ? (
                    <ChevronDown className="w-4 h-4 mr-2 text-white" />
                  ) : (
                    <ChevronRight className="w-4 h-4 mr-2 text-white" />
                  )}
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
              <td
                onClick={() => onCopy(dataSource.id)}
                className="text-ring cursor-pointer"
              >
                {dataSource.id}
              </td>
              <td className="p-2">
                {dataSource.indexStatus === KnowledgeIndexStatus.FAILED
                  ? "Failed"
                  : Math.round(dataSource.indexPercentage) + "%"}
              </td>
              <td className="p-2">{dataSource.indexStatus}</td>
              <td className="p-2">{dataSource.indexPercentage}</td>
              <td className="p-2">NA</td>
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
            {expanded.includes(dataSource.id) &&
              dataSource.knowledges.map(({ knowledge }: any) => (
                <tr key={knowledge.id} className="items-center my-2 text-sm">
                  <td className="p-2 pl-10">
                    <div className="max-w-sm truncate">
                      {knowledge.metadata?.indexingRunId ? (
                        <Link
                          target="_blank"
                          href={`https://console.apify.com/organization/Xn4BErd8aMtmstvY2/actors/moJRLRc85AitArpNN/runs/${knowledge.metadata.indexingRunId}`}
                          className="text-ring"
                        >
                          {knowledge.name}
                        </Link>
                      ) : (
                        knowledge.name
                      )}
                    </div>
                  </td>
                  <td className="p-2">
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
                  <td className="p-2">{knowledge.type}</td>
                  <td className="p-2">
                    {knowledge.lastIndexedAt
                      ? format(
                          new Date(knowledge.lastIndexedAt),
                          "h:mma M/d/yyyy "
                        )
                      : null}
                  </td>
                  <td
                    onClick={() => onCopy(dataSource.id)}
                    className="text-ring cursor-pointer"
                  >
                    {knowledge.id}
                  </td>
                  <td className="p-2">NA</td>
                  <td className="p-2">
                    {knowledge.blobUrl ? (
                      <a
                        href={knowledge.blobUrl}
                        target="_blank"
                        className="text-ring"
                      >
                        {knowledge.indexStatus}
                      </a>
                    ) : (
                      knowledge.indexStatus
                    )}
                  </td>
                  <td className="p-2">{knowledge.metadata?.documentCount}</td>
                  <td className="p-2">{knowledge.metadata?.totalTokenCount}</td>
                  <td className="p-2 text-center">
                    <Button
                      type="button"
                      variant="outline"
                      disabled={!!removing}
                      onClick={() => removeDataSource(knowledge.id)}
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
          </>
        ))}
      </Table>
    </div>
  );
};
