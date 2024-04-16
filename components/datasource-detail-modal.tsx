"use client";

import { Button } from "@/components/ui/button";
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";
import { useConfirmModal } from "@/hooks/use-confirm-modal";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { Loader, MinusCircle, RefreshCcw } from "lucide-react";
import { useState } from "react";
import { Table } from "./table";

type Props = {
  dataSource: any;
  onClose: () => void;
  toast: any;
  setDataSource: (dataSource: any) => void;
  aiId: string;
  fetchDataSources: () => void;
};

export const DataSourceDetailModal = ({
  dataSource,
  onClose,
  toast,
  setDataSource,
  aiId,
  fetchDataSources,
}: Props) => {
  const [removing, setRemoving] = useState(false);
  const [refreshing, setRefreshing] = useState("");
  const confirmModal = useConfirmModal();

  if (!dataSource) return null;

  const refreshDataSource = async (id: string) => {
    setRefreshing(id);
    try {
      await axios.put(`/api/v1/data-sources/${id}/refresh`);

      toast({ description: "Data source refresh request accepted." });
      fetchDataSources();
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

  const removeDataSource = async () => {
    setRemoving(true);
    try {
      await axios.delete(`/api/v1/data-sources/${dataSource.id}/`);

      setDataSource((current: any) =>
        current.filter((i: any) => i.id !== dataSource.id)
      );
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
    setRemoving(false);
    onClose();
  };

  const disconnectDataSource = async () => {
    setRemoving(true);
    try {
      const ais = dataSource.ais
        .map((ai: any) => ai.ai.id)
        .filter((id: any) => id !== aiId);
      await axios.patch(`/api/v1/data-sources/${dataSource.id}`, {
        ais,
      });
      setDataSource((current: any) =>
        current.filter((i: any) => i.id !== dataSource.id)
      );
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
    setRemoving(false);
    onClose();
  };

  const onRemove = async () => {
    if (dataSource.ais.length > 1) {
      confirmModal.onOpen(
        "Remove Data Source?",
        <div>
          <div>
            Are you sure you want to remove {dataSource.name} from this AI?
          </div>
          <div>Other AIs will continue using this data source.</div>
        </div>,
        () => disconnectDataSource()
      );
    } else {
      confirmModal.onOpen(
        "Delete Data Source?",
        <div>
          <div>Are you sure you want to delete {dataSource.name}?</div>
          <div>This action cannot be undone.</div>
        </div>,
        () => removeDataSource()
      );
    }
  };

  return (
    <Dialog
      open={!!dataSource}
      onOpenChange={(isOpen) => {
        if (!isOpen) {
          onClose();
        }
      }}
    >
      <DialogContent className="max-w-[800px] overflow-auto">
        <DialogHeader className="space-y-4">
          <DialogTitle className="truncate max-w-[480px]">
            <div className="truncate max-w-[480px]">
              Details for {dataSource.name}
            </div>
          </DialogTitle>
        </DialogHeader>
        <Separator />
        <div className="flex justify-between w-full">
          <Button
            type="button"
            variant="outline"
            size="sm"
            disabled={!!removing}
            onClick={() => refreshDataSource(dataSource.id)}
            className="ml-2"
          >
            Refresh Data Source
            {refreshing === dataSource.id ? (
              <Loader className="w-4 h-4 spinner ml-2" />
            ) : (
              <RefreshCcw className="w-4 h-4 text-green ml-2" />
            )}
          </Button>
          <Button
            type="button"
            variant="destructive"
            size="sm"
            disabled={!!removing}
            onClick={() => onRemove()}
          >
            {dataSource.ais.length > 1
              ? "Disconnect Data Source"
              : "Delete Data Source"}
            {removing ? (
              <Loader className="w-4 h-4 spinner ml-2 text-white" />
            ) : (
              <MinusCircle className="w-4 h-4 text-destructive ml-2 text-white" />
            )}
          </Button>
        </div>
        {dataSource.data.error && (
          <div className="text-destructive text-sm">
            Error: {dataSource.data.error}
          </div>
        )}
        {dataSource.knowledges.length ? (
          <Table
            headers={["Name", "Last Indexed", "Status", "Progress"]}
            className="w-full my-4 max-h-60"
          >
            {dataSource.knowledges.map(({ knowledge }: any) => (
              <>
                <tr key={knowledge.id} className="p-2">
                  <td className="p-2 cursor-pointer hover:text-ring">
                    <a href={knowledge.documentsBlobUrl} target="_blank">
                      {knowledge.name}
                    </a>
                  </td>
                  <td className="p-2">
                    {dataSource.lastIndexedAt
                      ? format(
                          new Date(dataSource.lastIndexedAt),
                          "h:mma M/d/yyyy "
                        )
                      : "Never"}
                  </td>
                  <td className="p-2">{knowledge.indexStatus}</td>
                  <td className="p-2">
                    {knowledge.indexPercentage ? (
                      <div>{Math.round(knowledge.indexPercentage)}%</div>
                    ) : (
                      "0%"
                    )}
                  </td>
                </tr>
                {knowledge.metadata?.errors && (
                  <tr key={`error-${knowledge.id}`} className="p-2">
                    <td colSpan={4} className="p-2 text-destructive text-sm">
                      Error:&nbsp;
                      {(
                        Object.values(knowledge.metadata.errors) as string[]
                      ).find((x: string) => x !== undefined)}
                    </td>
                  </tr>
                )}
              </>
            ))}
          </Table>
        ) : null}
      </DialogContent>
    </Dialog>
  );
};
