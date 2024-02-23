"use client";

import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";
import { format } from "date-fns";
import { Table } from "./table";

type Props = {
  dataSource: any;
  onClose: () => void;
};

export const DataSourceDetailModal = ({ dataSource, onClose }: Props) => {
  if (!dataSource) return null;

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
                    <a href={knowledge.blobUrl} target="_blank">
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
