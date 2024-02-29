"use client";
import { Button } from "@/components/ui/button";
import {
  FormDescription,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { useToast } from "@/components/ui/use-toast";
import { cn } from "@/src/lib/utils";
import { KnowledgeIndexStatus } from "@prisma/client";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { Coffee, Loader, Unplug } from "lucide-react";
import { useEffect, useState } from "react";
import { Table } from "./table";
import { Avatar, AvatarImage } from "./ui/avatar";
import { Tooltip } from "./ui/tooltip";

interface Props {
  goBack: () => void;
  form: any;
}

export const ConnectKnowledge = ({ goBack, form }: Props) => {
  const [connecting, setConnecting] = useState(false);
  const [dataSources, setDataSources] = useState<any[]>([]);
  const [loading, setLoading] = useState<boolean>(true);
  const [selected, setSelected] = useState<any | null>(null);
  const { toast } = useToast();

  const aiId = form.getValues("id");

  const fetchDataSources = async () => {
    setLoading(true);
    try {
      const response = await axios.get("/api/v1/data-sources");
      setDataSources(
        response.data.data.filter(
          (ds: any) => !ds.ais.some((ai: any) => ai.ai.id === aiId)
        )
      );
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
  }, []);

  const connect = async () => {
    if (!selected) {
      return;
    }
    setConnecting(true);

    try {
      const currentAis = selected.ais.map((ai: any) => ai.ai.id);
      await axios.patch(`/api/v1/data-sources/${selected.id}`, {
        ais: [...currentAis, aiId],
      });
      goBack();
    } catch (error) {
      toast({
        variant: "destructive",
        description: "Something went wrong",
      });
    }
    setConnecting(false);
  };

  return (
    <div className="w-full p-6 bg-accent/30">
      <FormItem>
        <FormLabel>Connect Knowledge</FormLabel>
        <FormDescription>
          Link existing knowledge you created for another AI.
        </FormDescription>
        <FormMessage />
      </FormItem>
      <Table
        headers={["Name", "AI", "Last Indexed", "Progress"]}
        className="w-full my-4 max-h-60"
      >
        {dataSources.map((dataSource: any) => (
          <tr
            key={dataSource.id}
            className={cn(
              "items-center my-2 text-sm hover:bg-ring/10",
              selected && selected.id === dataSource.id && "bg-ring/10"
            )}
            onClick={() => setSelected(dataSource)}
          >
            <td className="p-2">
              {dataSource.name.length > 30 ? (
                <Tooltip content={dataSource.name} className="cursor-default">
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
              {dataSource.ais.map((ai: any, index: string) => (
                <div key={`ai-${index}-${dataSource.id}`}>
                  <Tooltip content={ai.ai.name} className="cursor-default">
                    <Avatar className="h-6 w-6 mr-2">
                      <AvatarImage src={ai.ai.src} crop="w_48,h_48" />
                    </Avatar>
                  </Tooltip>
                </div>
              ))}
            </td>
            <td className="p-2">
              {dataSource.lastIndexedAt
                ? format(new Date(dataSource.lastIndexedAt), "h:mma M/d/yyyy ")
                : null}
            </td>
            <td className="p-2">
              <div className="flex items-center">
                {dataSource.indexStatus === KnowledgeIndexStatus.FAILED
                  ? "Failed"
                  : Math.round(dataSource.indexPercentage) + "%"}
              </div>
            </td>
          </tr>
        ))}
      </Table>
      {!loading && dataSources.length === 0 ? (
        <div className="flex items-center my-2 w-full">
          <div className="mx-auto flex p-4 bg-background rounded-lg">
            <Coffee className="w-6 h-6 mr-2" />
            <p>You don&apos;t have any existing data sources.</p>
          </div>
        </div>
      ) : null}
      {loading && dataSources.length === 0 ? (
        <div className="flex items-center my-2 w-full">
          <div className="mx-auto">
            <Loader className="w-8 h-8 spinner" />
          </div>
        </div>
      ) : null}
      <div className="flex flex-row-reverse w-full mt-4">
        <Button
          type="button"
          disabled={connecting || !selected}
          onClick={() => connect()}
          variant="ring"
        >
          Connect
          {connecting ? (
            <Loader className="w-4 h-4 ml-2 spinner" />
          ) : (
            <Unplug className="w-4 h-4 ml-2" />
          )}
        </Button>
      </div>
    </div>
  );
};
