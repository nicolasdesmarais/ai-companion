"use client";

import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";
import { format } from "date-fns";

type Props = {
  dataSource: any;
  onClose: () => void;
};

export const DataSourceDetailModal = ({ dataSource, onClose }: Props) => {
  if (!dataSource) return null;
  console.log(dataSource);
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
          <DialogTitle className="text-center">
            {dataSource.name} Details
          </DialogTitle>
        </DialogHeader>
        <Separator />
        {dataSource.knowledges.map(({ knowledge }: any) => (
          <div key={knowledge.id} className="p-2">
            <div className="flex justify-between">
              <div>{knowledge.name}</div>
              <div>
                {format(new Date(dataSource.lastIndexedAt), "h:mma M/d/yyyy ")}
              </div>
              <div>{knowledge.indexStatus}</div>
              {knowledge.metadata.percentComplete &&
                knowledge.indexStatus === "PARTIALLY_COMPLETED" && (
                  <div>{knowledge.metadata.percentComplete.toFixed(2)}%</div>
                )}
            </div>
            {knowledge.metadata.errors && (
              <div className="text-destructive text-sm">
                Error:&nbsp;
                {(Object.values(knowledge.metadata.errors) as string[]).find(
                  (x: string) => x !== undefined
                )}
              </div>
            )}
          </div>
        ))}
      </DialogContent>
    </Dialog>
  );
};
