"use client";
import { useEffect } from "react";
import { Table } from "@/components/table";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import {
  ChevronLeft,
  Coffee,
  FileUp,
  Globe,
  Loader,
  MinusCircle,
  Server,
} from "lucide-react";
import { usePathname, useRouter } from "next/navigation";
import { useState } from "react";
import DataSourceCard from "./datasource-card";
import { DataSourceTypes } from "./datasource-types";
import { FileUploadKnowledge } from "./file-upload-knowledge";
import { GoogleDriveForm } from "./google-drive-knowledge";
import { WebUrlsForm } from "./web-urls-knowledge-form";
import { KnowledgeIndexStatus } from "@prisma/client";
import { Banner } from "./ui/banner";

export const SuperDataSources = () => {
  const [dataSources, setDataSources] = useState<any[]>([]);
  const [removing, setRemoving] = useState("");
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

  return (
    <div>
      <Table
        headers={["NAME", "AIs", "TYPE", "LAST MODIFIED", "Progress", "Remove"]}
        className="w-full my-4 max-h-60"
      >
        {dataSources.map((dataSource: any) => (
          <>
            <tr key={dataSource.id} className="items-center my-2 text-sm">
              <td className="p-2 ">
                <div className="max-w-sm truncate">{dataSource.name}</div>
              </td>
              <td className="p-2">
                {dataSource.ais.map((ai: any) => ai.ai.name)}
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
            {dataSource.knowledges.map(({ knowledge }: any) => (
              <tr key={knowledge.id} className="items-center my-2 text-sm">
                <td className="p-2 pl-10">
                  <div className="max-w-sm truncate">{knowledge.name}</div>
                </td>
                <td className="p-2">
                  {dataSource.ais.map((ai: any) => ai.ai.name)}
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
                <td className="p-2">
                  {knowledge.indexStatus === KnowledgeIndexStatus.FAILED
                    ? "Failed"
                    : Math.round(knowledge.indexPercentage) + "%"}
                </td>
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
